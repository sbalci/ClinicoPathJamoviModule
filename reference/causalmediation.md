# Causal Mediation Analysis

Causal Mediation Analysis examines the mechanisms through which an
exposure affects an outcome. This module provides three tiers:

## Usage

``` r
causalmediation(
  data,
  outcome,
  treatment,
  mediator = NULL,
  mediators = NULL,
  covariates = NULL,
  mediation_tier = "basic",
  boot_samples = 1000,
  conf_level = 0.95,
  mediator_model = "lm",
  outcome_model = "lm",
  multiple_mediators = NULL,
  show_dag = TRUE,
  cma_estimation = "rb",
  hd_method = "hdmax2",
  hd_fdr_threshold = 0.1,
  hd_top_mediators = 20,
  hd_penalty = "lasso",
  hd_parallel = TRUE,
  show_decomposition = TRUE,
  show_prop_mediated = TRUE,
  sensitivity_analysis = FALSE,
  rho_values = "0, 0.1, 0.2, 0.3",
  plot_effects = TRUE,
  plot_hd_manhattan = TRUE,
  plot_hd_volcano = TRUE,
  interaction_term = FALSE,
  random_seed = 42
)
```

## Arguments

- data:

  the data as a data frame

- outcome:

  Outcome variable (Y)

- treatment:

  Treatment or exposure variable (X)

- mediator:

  Mediator variable for basic/comprehensive mediation (M)

- mediators:

  Multiple mediators for high-dimensional analysis

- covariates:

  Confounding variables (Z)

- mediation_tier:

  Tier of mediation analysis to perform

- boot_samples:

  Number of bootstrap samples for confidence intervals

- conf_level:

  Confidence level for intervals

- mediator_model:

  Model type for mediator regression

- outcome_model:

  Model type for outcome regression

- multiple_mediators:

  Multiple mediators for CMAverse analysis

- show_dag:

  Display directed acyclic graph

- cma_estimation:

  Estimation method for CMAverse

- hd_method:

  Method for high-dimensional mediation

- hd_fdr_threshold:

  False discovery rate threshold for HD mediation

- hd_top_mediators:

  Number of top mediators to display

- hd_penalty:

  Penalization method for variable selection

- hd_parallel:

  Enable parallel computing for HD analysis

- show_decomposition:

  Display total, direct, and indirect effects

- show_prop_mediated:

  Calculate proportion of effect mediated

- sensitivity_analysis:

  Perform sensitivity analysis for unmeasured confounding

- rho_values:

  Correlation values for sensitivity analysis

- plot_effects:

  Generate forest plot of effects

- plot_hd_manhattan:

  Generate Manhattan plot for HD mediation

- plot_hd_volcano:

  Generate volcano plot for HD mediation

- interaction_term:

  Model treatment-mediator interaction

- random_seed:

  Random seed for reproducibility

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$instructions`       |     |     |     |     | a html         |
| `results$tierInfo`           |     |     |     |     | a preformatted |
| `results$effectsTable`       |     |     |     |     | a table        |
| `results$proportionMediated` |     |     |     |     | a table        |
| `results$hdMediatorsTable`   |     |     |     |     | a table        |
| `results$hdSummary`          |     |     |     |     | a html         |
| `results$cmaResults`         |     |     |     |     | a html         |
| `results$dagPlot`            |     |     |     |     | an image       |
| `results$sensitivityTable`   |     |     |     |     | a table        |
| `results$sensitivityPlot`    |     |     |     |     | an image       |
| `results$effectsPlot`        |     |     |     |     | an image       |
| `results$manhattanPlot`      |     |     |     |     | an image       |
| `results$volcanoPlot`        |     |     |     |     | an image       |
| `results$modelInfo`          |     |     |     |     | a html         |
| `results$interpretation`     |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$effectsTable$asDF`

`as.data.frame(results$effectsTable)`

## Details

1.  **Basic Mediation**: Traditional mediation analysis (mediation
    package)

2.  **Comprehensive Mediation**: Multiple mediators with DAGs (CMAverse)

3.  **HIGH-DIMENSIONAL Mediation**: Omics-scale mediators (hdmax2)

Applications: Understanding pathways in cancer biology, clinical trials,
epidemiology, and precision medicine research.
