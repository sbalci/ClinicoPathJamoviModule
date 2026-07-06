# Hierarchical Bayesian Diagnostic Models

Hierarchical Bayesian models for multi-center diagnostic studies.
Implements bivariate meta-analysis of diagnostic accuracy with
between-study heterogeneity and correlation structures.

## Usage

``` r
hierarchicalbayes(
  tp,
  fp,
  fn,
  tn,
  study_id,
  study_covariates = NULL,
  model_type = "bivariate_normal",
  correlation_model = "unstructured",
  prior_specification = "weakly_informative",
  mcmc_chains = 4,
  mcmc_iterations = 20000,
  warmup_iterations = 10000,
  thin_interval = 1,
  adapt_delta = 0.95,
  max_treedepth = 15,
  credible_level = 0.95,
  prediction_interval = TRUE,
  cross_validation = FALSE,
  posterior_predictive = TRUE,
  convergence_diagnostics = TRUE,
  model_comparison = TRUE,
  forest_plots = TRUE,
  sroc_curves = TRUE,
  trace_plots = FALSE,
  density_plots = TRUE,
  pairs_plots = FALSE,
  shrinkage_plots = FALSE,
  meta_regression = FALSE,
  outlier_detection = TRUE,
  sensitivity_analysis = FALSE
)
```

## Arguments

- tp:

  True positive counts by study/center

- fp:

  False positive counts by study/center

- fn:

  False negative counts by study/center

- tn:

  True negative counts by study/center

- study_id:

  Unique identifier for each study/center

- study_covariates:

  Covariates explaining between-study heterogeneity

- model_type:

  Statistical model for hierarchical meta-analysis

- correlation_model:

  Assumed correlation between diagnostic measures

- prior_specification:

  Method for specifying prior distributions

- mcmc_chains:

  Parallel chains for convergence assessment

- mcmc_iterations:

  Total iterations including warmup

- warmup_iterations:

  Burn-in period for MCMC sampling

- thin_interval:

  Thinning to reduce autocorrelation

- adapt_delta:

  Controls step size adaptation in HMC

- max_treedepth:

  Controls exploration in NUTS algorithm

- credible_level:

  Level for Bayesian credible intervals

- prediction_interval:

  Whether to compute predictive distributions

- cross_validation:

  Whether to perform cross-validation

- posterior_predictive:

  Whether to perform posterior predictive checking

- convergence_diagnostics:

  Whether to assess MCMC convergence

- model_comparison:

  Whether to compute model comparison metrics

- forest_plots:

  Whether to create forest plots

- sroc_curves:

  Whether to plot SROC curves

- trace_plots:

  Whether to create MCMC trace plots

- density_plots:

  Whether to create posterior density plots

- pairs_plots:

  Whether to create parameter correlation plots

- shrinkage_plots:

  Whether to plot shrinkage effects

- meta_regression:

  Whether to conduct meta-regression

- outlier_detection:

  Whether to perform outlier analysis

- sensitivity_analysis:

  Whether to perform sensitivity analysis

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$modelSpecification`      |     |     |     |     | a table  |
| `results$studySummary`            |     |     |     |     | a table  |
| `results$pooledEstimates`         |     |     |     |     | a table  |
| `results$hierarchicalParameters`  |     |     |     |     | a table  |
| `results$heterogeneityAssessment` |     |     |     |     | a table  |
| `results$correlationAnalysis`     |     |     |     |     | a table  |
| `results$metaRegressionResults`   |     |     |     |     | a table  |
| `results$outlierAnalysis`         |     |     |     |     | a table  |
| `results$convergenceDiagnostics`  |     |     |     |     | a table  |
| `results$modelComparison`         |     |     |     |     | a table  |
| `results$posteriorPredictive`     |     |     |     |     | a table  |
| `results$forestPlotSensitivity`   |     |     |     |     | an image |
| `results$forestPlotSpecificity`   |     |     |     |     | an image |
| `results$srocCurvePlot`           |     |     |     |     | an image |
| `results$tracePlots`              |     |     |     |     | an image |
| `results$densityPlots`            |     |     |     |     | an image |
| `results$pairsPlots`              |     |     |     |     | an image |
| `results$shrinkagePlots`          |     |     |     |     | an image |
| `results$methodExplanation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSpecification$asDF`

`as.data.frame(results$modelSpecification)`
