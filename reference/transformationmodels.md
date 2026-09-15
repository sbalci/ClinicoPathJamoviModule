# Transformation Models

Implements transformation models providing a unified framework for
various survival analysis approaches through transformation functions.
Includes linear transformation models, Box-Cox transformations, and
non-parametric transformations, enabling flexible modeling of survival
data with automatic transformation selection and validation.

## Usage

``` r
transformationmodels(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  transformation = "boxcox",
  distribution = "normal",
  method = "ml",
  support = "automatic",
  lambda_search = TRUE,
  lambda_range_min = -2,
  lambda_range_max = 2,
  stratify_variable,
  weights_variable,
  confidence_level = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  transformation_validation = TRUE,
  model_selection = FALSE,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_transformation = TRUE,
  show_diagnostics = TRUE,
  show_comparison = TRUE,
  show_transformation_plots = TRUE,
  show_residual_plots = TRUE,
  show_survival_plots = TRUE,
  show_qq_plots = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Survival time or follow-up duration variable

- outcome:

  Event indicator variable (0/1, FALSE/TRUE, or factor)

- covariates:

  Predictor variables for the transformation model

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- transformation:

  Type of transformation function to apply

- distribution:

  Error distribution assumption

- method:

  Parameter estimation method

- support:

  Specification of transformation support

- lambda_search:

  Automatic search for optimal Box-Cox lambda parameter

- lambda_range_min:

  Minimum value for lambda parameter search

- lambda_range_max:

  Maximum value for lambda parameter search

- stratify_variable:

  Variable for stratified analysis

- weights_variable:

  Observation weights variable

- confidence_level:

  Confidence level for intervals

- max_iterations:

  Maximum number of iterations

- convergence_tolerance:

  Convergence tolerance for estimation

- transformation_validation:

  Validate transformation assumptions

- model_selection:

  Automatic selection of best transformation

- bootstrap_ci:

  Compute bootstrap confidence intervals

- bootstrap_samples:

  Number of bootstrap samples

- show_model_summary:

  Display comprehensive model summary

- show_coefficients:

  Display coefficient estimates table

- show_transformation:

  Display transformation function analysis

- show_diagnostics:

  Display model diagnostics

- show_comparison:

  Compare different transformation models

- show_transformation_plots:

  Display transformation function plots

- show_residual_plots:

  Display residual diagnostic plots

- show_survival_plots:

  Display survival curves

- show_qq_plots:

  Display quantile-quantile plots

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`           |     |     |     |     | a table  |
| `results$coefficients`           |     |     |     |     | a table  |
| `results$transformationAnalysis` |     |     |     |     | a table  |
| `results$diagnostics`            |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$transformationInfo`     |     |     |     |     | a table  |
| `results$lambdaSearch`           |     |     |     |     | a table  |
| `results$validationResults`      |     |     |     |     | a table  |
| `results$transformationPlots`    |     |     |     |     | an image |
| `results$residualPlots`          |     |     |     |     | an image |
| `results$survivalPlots`          |     |     |     |     | an image |
| `results$qqPlots`                |     |     |     |     | an image |
| `results$summaryTable`           |     |     |     |     | a html   |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
