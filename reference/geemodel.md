# Generalized Estimating Equations

Generalized Estimating Equations for analyzing correlated/clustered
data. Handles repeated measures, longitudinal data, multi-site studies
with marginal model approach. Essential for pathology studies with
multiple samples per subject (e.g., bilateral organs, multiple biopsies
per patient).

## Usage

``` r
geemodel(
  data,
  outcome,
  predictors,
  cluster_id,
  time_var,
  family = "gaussian",
  corstr = "exchangeable",
  robust_se = TRUE,
  conf_level = 0.95,
  qic = TRUE,
  posthoc = FALSE,
  posthoc_adjust = "holm",
  diagnostics = TRUE,
  correlation_plot = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- outcome:

  outcome variable (dependent variable); can be continuous, binary, or
  count

- predictors:

  predictor variables (independent variables) for the model

- cluster_id:

  variable identifying clusters (e.g., patient ID, dog ID). Observations
  with the same ID are treated as correlated.

- time_var:

  for longitudinal data, specifies time ordering within clusters. Used
  with AR(1) correlation structure.

- family:

  distribution family and link function for the outcome variable.
  Gaussian for continuous, Binomial for binary, Poisson for counts.

- corstr:

  Working correlation structure within clusters: - Exchangeable:
  constant correlation between all pairs (recommended starting point) -
  AR(1): correlation decays with time lag (for longitudinal data) -
  Unstructured: estimates all pairwise correlations (requires many
  observations) - Independence: no correlation (equivalent to GLM)

- robust_se:

  sandwich estimator for standard errors (recommended). Provides valid
  inference even if correlation structure is misspecified.

- conf_level:

  confidence level for confidence intervals (default 95 percent)

- qic:

  Quasi-likelihood under Independence Model Criterion. Lower QIC
  indicates better model fit. Use for comparing models.

- posthoc:

  compare levels of categorical predictors using estimated marginal
  means

- posthoc_adjust:

  multiple testing correction for post-hoc comparisons

- diagnostics:

  display model diagnostics including number of clusters, observations
  per cluster

- correlation_plot:

  visualize the estimated within-cluster correlation structure

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$modelInfo`          |     |     |     |     | a table  |
| `results$coefficientsTable`  |     |     |     |     | a table  |
| `results$qicTable`           |     |     |     |     | a table  |
| `results$posthocTable`       |     |     |     |     | a table  |
| `results$diagnosticsTable`   |     |     |     |     | a table  |
| `results$correlationPlot`    |     |     |     |     | an image |
| `results$methodologyNote`    |     |     |     |     | a html   |
| `results$interpretationNote` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelInfo$asDF`

`as.data.frame(results$modelInfo)`

## Details

**Key Features:**

- Handles binary, continuous, and count outcomes

- Multiple correlation structures (exchangeable, AR-1, unstructured)

- Robust standard errors (sandwich estimator)

- Post-hoc pairwise comparisons

- Model selection with QIC

**Clinical Applications:**

- Multiple samples per patient

- Bilateral organs (eyes, kidneys)

- Repeated measures over time

- Multi-site studies

- Clustered randomized trials
