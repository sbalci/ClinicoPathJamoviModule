# Principal Component Cox Models

Principal Component Analysis for Cox regression with high-dimensional
predictors. Reduces dimensionality while preserving survival-relevant
information using supervised PCA.

## Usage

``` r
pcacox(
  data,
  time,
  status,
  outcomeLevel,
  censorLevel,
  predictors,
  suitabilityCheck = TRUE,
  clinical_vars,
  pca_method = "supervised",
  n_components = 5,
  component_selection = "cv",
  cv_folds = 10,
  sparse_parameter = 0.1,
  confidence_level = 0.95,
  variance_threshold = 0.8,
  scaling = TRUE,
  centering = TRUE,
  survival_weighting = TRUE,
  permutation_test = FALSE,
  n_permutations = 100,
  bootstrap_validation = TRUE,
  n_bootstrap = 100,
  plot_scree = TRUE,
  plot_loadings = TRUE,
  plot_biplot = TRUE,
  plot_survival = TRUE,
  risk_score = TRUE,
  show_model_comparison = FALSE,
  pathway_analysis = FALSE,
  feature_importance = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Survival time variable

- status:

  Event status variable

- outcomeLevel:

  Level of status variable indicating event occurred

- censorLevel:

  Level of status variable indicating censored (no event)

- predictors:

  Variables for principal component analysis

- suitabilityCheck:

  assess if data is suitable for the selected PCA Cox model

- clinical_vars:

  Clinical predictors to retain in model

- pca_method:

  PCA methodology for dimensionality reduction

- n_components:

  Number of PCs to include in Cox model

- component_selection:

  Selection criteria for PCs

- cv_folds:

  K-fold CV parameter

- sparse_parameter:

  Sparsity alpha/beta parameter

- confidence_level:

  Confidence interval width

- variance_threshold:

  Proportion of variance threshold

- scaling:

  Center and scale variables

- centering:

  Center variables to zero mean

- survival_weighting:

  Use survival information in PCA

- permutation_test:

  Test component significance via permutation

- n_permutations:

  Permutation sample size

- bootstrap_validation:

  Assess model stability via bootstrap

- n_bootstrap:

  Bootstrap sample size

- plot_scree:

  Generate scree plot

- plot_loadings:

  Generate loading visualizations

- plot_biplot:

  Generate biplot visualization

- plot_survival:

  Generate survival stratification plots

- risk_score:

  Calculate combined risk score

- show_model_comparison:

  Calculate and compare sequential component additions

- pathway_analysis:

  Group features by their dominant PC and rank by survival-weighted
  importance

- feature_importance:

  Rank features by contribution

## Value

A results object containing:

|                                  |     |     |     |     |                |
|----------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                |     |     |     |     | a preformatted |
| `results$todo`                   |     |     |     |     | a html         |
| `results$suitabilityReport`      |     |     |     |     | a html         |
| `results$summary`                |     |     |     |     | a html         |
| `results$pcaSummary`             |     |     |     |     | a table        |
| `results$coxResults`             |     |     |     |     | a table        |
| `results$componentLoadings`      |     |     |     |     | a table        |
| `results$modelPerformance`       |     |     |     |     | a table        |
| `results$featureImportance`      |     |     |     |     | a table        |
| `results$riskScore`              |     |     |     |     | a table        |
| `results$modelComparison`        |     |     |     |     | a table        |
| `results$screePlot`              |     |     |     |     | an image       |
| `results$loadingsPlot`           |     |     |     |     | an image       |
| `results$biplot`                 |     |     |     |     | an image       |
| `results$survivalPlot`           |     |     |     |     | an image       |
| `results$crossValidation`        |     |     |     |     | a html         |
| `results$bootstrapValidation`    |     |     |     |     | a html         |
| `results$permutationTest`        |     |     |     |     | a html         |
| `results$pathwayAnalysis`        |     |     |     |     | a html         |
| `results$technicalDetails`       |     |     |     |     | a html         |
| `results$clinicalInterpretation` |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$pcaSummary$asDF`

`as.data.frame(results$pcaSummary)`
