# Partial Least Squares Cox Models

Partial Least Squares Cox regression for high-dimensional survival data.
Combines PLS dimensionality reduction with Cox proportional hazards
modeling for analysis of genomic, proteomic, and other high-dimensional
datasets.

## Usage

``` r
plscox(
  data,
  time,
  status,
  outcomeLevel,
  censorLevel,
  predictors,
  pls_components = 5,
  cross_validation = "k10",
  component_selection = "cv_loglik",
  scaling_method = "standardize",
  tolerance = 1e-06,
  tie_method = "efron",
  sparse_pls = FALSE,
  limQ2set = 0.0975,
  pvals_expli = FALSE,
  alpha_pvals_expli = 0.05,
  bootstrap_validation = FALSE,
  n_bootstrap = 200,
  permutation_test = FALSE,
  n_permutations = 100,
  plot_components = TRUE,
  plot_loadings = TRUE,
  plot_scores = TRUE,
  plot_validation = TRUE,
  plot_survival = TRUE,
  risk_groups = 3,
  confidence_intervals = TRUE,
  feature_importance = TRUE,
  prediction_accuracy = TRUE,
  suitabilityCheck = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  .

- status:

  .

- outcomeLevel:

  Level of `status` considered as the event. For binary factor outcomes,
  if left empty the second observed level is used; for numeric binary
  outcomes, the larger observed value is used (or 1 for 0/1 coding).

- censorLevel:

  Level of `status` considered as censored (no event). Together with
  `outcomeLevel`, this defines a strict two-level encoding: rows whose
  status matches neither level are treated as missing and excluded.

- predictors:

  .

- pls_components:

  Number of PLS components to extract

- cross_validation:

  Cross-validation method for component selection

- component_selection:

  Method for selecting optimal number of components

- scaling_method:

  Method for scaling predictor variables

- tolerance:

  Tolerance for algorithm convergence (jamovi default: 1e-06, plsRcox
  default: 1e-12)

- tie_method:

  Method for handling tied event times in Cox model (plsRcox default:
  efron)

- sparse_pls:

  Enable sparse PLS for automatic variable selection (plsRcox default:
  false)

- limQ2set:

  Q-squared threshold for PLS component stopping criterion (plsRcox
  default: 0.0975)

- pvals_expli:

  Use p-value based predictor selection during PLS fitting (plsRcox
  default: false)

- alpha_pvals_expli:

  Significance level for p-value based variable selection (plsRcox
  default: 0.05)

- bootstrap_validation:

  Perform bootstrap validation of model performance

- n_bootstrap:

  Number of bootstrap replications

- permutation_test:

  Perform permutation test for variable importance

- n_permutations:

  Number of permutations for significance testing

- plot_components:

  Create PLS component visualization plots

- plot_loadings:

  Display variable loadings for PLS components

- plot_scores:

  Show component scores and survival relationships

- plot_validation:

  Display cross-validation curves for component selection

- plot_survival:

  Generate risk-stratified survival curves

- risk_groups:

  Number of risk groups for survival stratification

- confidence_intervals:

  Calculate confidence intervals for hazard ratios

- feature_importance:

  Calculate and display variable importance scores

- prediction_accuracy:

  Assess model prediction accuracy using C-index and other metrics

- suitabilityCheck:

  Run a comprehensive data suitability assessment before analysis.
  Checks sample size, events-per-variable ratio, multicollinearity, and
  whether regularization is needed.

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$todo`               |     |     |     |     | a html   |
| `results$suitabilityReport`  |     |     |     |     | a html   |
| `results$modelSummary`       |     |     |     |     | a html   |
| `results$componentSelection` |     |     |     |     | a table  |
| `results$modelCoefficients`  |     |     |     |     | a table  |
| `results$variableLoadings`   |     |     |     |     | a table  |
| `results$modelPerformance`   |     |     |     |     | a table  |
| `results$riskStratification` |     |     |     |     | a table  |
| `results$componentPlot`      |     |     |     |     | an image |
| `results$loadingsPlot`       |     |     |     |     | an image |
| `results$scoresPlot`         |     |     |     |     | an image |
| `results$validationPlot`     |     |     |     |     | an image |
| `results$survivalPlot`       |     |     |     |     | an image |
| `results$bootstrapResults`   |     |     |     |     | a html   |
| `results$permutationResults` |     |     |     |     | a html   |
| `results$clinicalGuidance`   |     |     |     |     | a html   |
| `results$technicalNotes`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$componentSelection$asDF`

`as.data.frame(results$componentSelection)`
