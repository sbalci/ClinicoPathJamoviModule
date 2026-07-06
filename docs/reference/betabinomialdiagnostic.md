# Beta-Binomial Diagnostic Accuracy Models

Beta-binomial models for overdispersed diagnostic accuracy data. Handles
heterogeneity in sensitivity and specificity across studies or centers
using beta-binomial distributions with flexible correlation structures.

## Usage

``` r
betabinomialdiagnostic(
  test_result,
  disease_status,
  study_id = NULL,
  covariates = NULL,
  overdispersion_model = "beta_binomial",
  correlation_structure = "independent",
  estimation_method = "maximum_likelihood",
  heterogeneity_test = TRUE,
  tau_squared_method = "dersimonian_laird",
  confidence_level = 0.95,
  alpha_prior = 1,
  beta_prior = 1,
  alpha_prior_spec = 1,
  beta_prior_spec = 1,
  mcmc_iterations = 10000,
  burnin_iterations = 2000,
  thin_factor = 5,
  convergence_diagnostics = TRUE,
  prediction_intervals = TRUE,
  forest_plot = TRUE,
  summary_roc_curve = TRUE,
  residual_plots = FALSE,
  influence_diagnostics = FALSE,
  publication_bias = FALSE,
  subgroup_analysis = FALSE,
  robust_variance = FALSE,
  finite_sample_correction = TRUE
)
```

## Arguments

- test_result:

  Test result variable

- disease_status:

  Gold standard disease status

- study_id:

  Clustering variable for multi-center studies

- covariates:

  Covariates for explaining between-study heterogeneity

- overdispersion_model:

  Statistical model for overdispersed diagnostic data

- correlation_structure:

  Assumed correlation structure

- estimation_method:

  Method for estimating model parameters

- heterogeneity_test:

  Whether to test for heterogeneity

- tau_squared_method:

  Estimator for τ² (heterogeneity variance)

- confidence_level:

  Confidence level for parameter estimates

- alpha_prior:

  Prior alpha for sensitivity in Bayesian models

- beta_prior:

  Prior beta for sensitivity in Bayesian models

- alpha_prior_spec:

  Prior alpha for specificity in Bayesian models

- beta_prior_spec:

  Prior beta for specificity in Bayesian models

- mcmc_iterations:

  MCMC iterations for posterior sampling

- burnin_iterations:

  MCMC burn-in iterations

- thin_factor:

  Keep every nth MCMC sample

- convergence_diagnostics:

  Whether to check MCMC convergence

- prediction_intervals:

  Whether to compute prediction intervals

- forest_plot:

  Whether to create forest plots

- summary_roc_curve:

  Whether to plot summary ROC curve

- residual_plots:

  Whether to create residual diagnostic plots

- influence_diagnostics:

  Whether to perform influence analysis

- publication_bias:

  Whether to assess publication bias

- subgroup_analysis:

  Whether to conduct subgroup analyses

- robust_variance:

  Whether to use robust standard errors

- finite_sample_correction:

  Whether to apply small sample corrections

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`             |     |     |     |     | a html   |
| `results$modelSummary`             |     |     |     |     | a table  |
| `results$overdispersionParameters` |     |     |     |     | a table  |
| `results$heterogeneityTests`       |     |     |     |     | a table  |
| `results$studyLevelResults`        |     |     |     |     | a table  |
| `results$pooledEstimates`          |     |     |     |     | a table  |
| `results$correlationResults`       |     |     |     |     | a table  |
| `results$convergenceDiagnostics`   |     |     |     |     | a table  |
| `results$subgroupAnalysis`         |     |     |     |     | a table  |
| `results$influenceAnalysis`        |     |     |     |     | a table  |
| `results$publicationBiasTests`     |     |     |     |     | a table  |
| `results$forestPlotSensitivity`    |     |     |     |     | an image |
| `results$forestPlotSpecificity`    |     |     |     |     | an image |
| `results$summaryROCPlot`           |     |     |     |     | an image |
| `results$residualPlots`            |     |     |     |     | an image |
| `results$convergencePlots`         |     |     |     |     | an image |
| `results$funnelPlot`               |     |     |     |     | an image |
| `results$methodExplanation`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
