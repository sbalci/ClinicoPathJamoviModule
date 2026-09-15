# High-Dimensional Cox Regression

Performs high-dimensional Cox proportional hazards regression using
advanced regularization methods (LASSO, Ridge, Elastic Net, Adaptive
LASSO) for survival data with many predictors. This method is essential
when the number of variables is large relative to the number of
observations, enabling variable selection and preventing overfitting in
genomic, proteomic, or other high-dimensional clinical datasets.

## Usage

``` r
highdimcox(
  data,
  elapsedtime,
  outcome,
  predictors,
  outcomeLevel,
  censorLevel,
  regularization_method = "elastic_net",
  alpha_value = 0.5,
  cv_method = "cv_1se",
  cv_folds = 10,
  stability_selection = FALSE,
  subsampling_iterations = 500,
  subsampling_ratio = 0.5,
  stability_threshold = 0.8,
  show_regularization_path = FALSE,
  show_cv_plot = FALSE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE,
  show_model_diagnostics = FALSE,
  showSummaries = FALSE,
  showExplanations = FALSE,
  suitabilityCheck = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Time variable for survival analysis

- outcome:

  Event indicator variable

- predictors:

  High-dimensional predictor variables

- outcomeLevel:

  Level of outcome variable indicating event occurred

- censorLevel:

  Level of outcome variable indicating censored (no event)

- regularization_method:

  Regularization method for high-dimensional Cox regression

- alpha_value:

  Alpha parameter for elastic net (0=ridge, 1=lasso)

- cv_method:

  Cross-validation method for lambda selection

- cv_folds:

  Number of cross-validation folds

- stability_selection:

  Perform stability selection for variable importance

- subsampling_iterations:

  Number of subsampling iterations for stability selection

- subsampling_ratio:

  Proportion of observations to sample in each stability selection
  iteration

- stability_threshold:

  Threshold for stability selection

- show_regularization_path:

  Display regularization path plot

- show_cv_plot:

  Display cross-validation error plot

- show_variable_importance:

  Display variable importance plot

- show_coefficients_table:

  Display selected coefficients table

- show_model_diagnostics:

  Display model diagnostic plots

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show methodology explanations

- suitabilityCheck:

  Run a comprehensive data suitability assessment before analysis.
  Checks sample size, events-per-variable ratio, multicollinearity, and
  whether regularization is needed.

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                  |     |     |     |     | a html   |
| `results$suitabilityReport`     |     |     |     |     | a html   |
| `results$modelSummary`          |     |     |     |     | a html   |
| `results$selectedVariables`     |     |     |     |     | a table  |
| `results$regularizationMetrics` |     |     |     |     | a table  |
| `results$stabilityResults`      |     |     |     |     | a table  |
| `results$regularizationPath`    |     |     |     |     | an image |
| `results$cvPlot`                |     |     |     |     | an image |
| `results$variableImportance`    |     |     |     |     | an image |
| `results$modelDiagnostics`      |     |     |     |     | an image |
| `results$stabilityPlot`         |     |     |     |     | an image |
| `results$analysisSummary`       |     |     |     |     | a html   |
| `results$methodExplanation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$selectedVariables$asDF`

`as.data.frame(results$selectedVariables)`
