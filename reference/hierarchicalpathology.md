# Hierarchical Mixed-Effects Models

Hierarchical (multilevel) mixed-effects models for analyzing nested
pathology data structures. Essential for WSI analysis with Patient \>
Slide \> ROI \> Cell hierarchies, and prevents Type I errors from
ignoring clustering effects.

## Usage

``` r
hierarchicalpathology(
  data,
  dependent,
  level_3,
  level_2,
  level_1,
  covariates,
  model_type = "linear",
  descriptives = TRUE,
  variance_components = TRUE,
  icc_analysis = TRUE,
  random_effects = TRUE,
  model_comparison = FALSE,
  diagnostics = TRUE,
  confidence_level = 0.95,
  optimizer = "bobyqa",
  max_iterations = 1000
)
```

## Arguments

- data:

  the data as a data frame

- dependent:

  The dependent variable from `data`, variable must be numeric

- level_3:

  Highest level grouping variable (e.g., Patient ID, Institution)

- level_2:

  Intermediate level grouping variable (e.g., Slide ID, Sample ID)

- level_1:

  Lowest level grouping variable (e.g., ROI ID, Field ID)

- covariates:

  Optional covariates for fixed effects (age, treatment, etc.)

- model_type:

  Type of mixed-effects model based on outcome variable

- descriptives:

  Show descriptive statistics by hierarchical level

- variance_components:

  Calculate and display variance components

- icc_analysis:

  Calculate Intraclass Correlation Coefficients

- random_effects:

  Display random effects variance and standard deviations

- model_comparison:

  Compare nested models with likelihood ratio tests

- diagnostics:

  Generate diagnostic plots and residual analysis

- confidence_level:

  Confidence level for parameter estimates and intervals

- optimizer:

  Optimization algorithm for model fitting

- max_iterations:

  Maximum number of iterations for model convergence

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions for hierarchical mixed-effects modeling |
| `results$descriptives` |  |  |  |  | Summary statistics for each hierarchical level |
| `results$modelresults` |  |  |  |  | Fixed effects parameter estimates and tests |
| `results$randomeffects` |  |  |  |  | Random effects variance components |
| `results$variancecomponents` |  |  |  |  | Partition of variance across hierarchical levels |
| `results$icc` |  |  |  |  | ICC values indicating clustering effects |
| `results$modelcomparison` |  |  |  |  | Likelihood ratio tests comparing nested models |
| `results$diagnosticplot` |  |  |  |  | Residual plots and diagnostic checks |
| `results$residualplot` |  |  |  |  | Residuals plotted by hierarchical level |
| `results$interpretation` |  |  |  |  | Clinical context and interpretation for hierarchical models |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptives$asDF`

`as.data.frame(results$descriptives)`
