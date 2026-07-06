# Bayesian Meta-Analysis

Performs Bayesian meta-analysis using hierarchical models for evidence
synthesis across multiple studies. This approach accounts for both
within-study and between-study variability using informative prior
distributions and MCMC sampling methods.

## Usage

``` r
bayesianmetaanalysis(
  data,
  effectSize,
  standardError,
  studyId,
  covariates,
  modelType = "random_effects",
  outcomeType = "continuous",
  priorType = "weakly_informative",
  mcmcChains = 4,
  mcmcIterations = 5000,
  warmupIterations = 2500,
  publicationBias = FALSE,
  posteriorPredictive = TRUE,
  plotForest = TRUE,
  plotPosterior = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- effectSize:

  the effect size variable (e.g., mean difference, log odds ratio)

- standardError:

  the standard error of the effect size

- studyId:

  variable identifying individual studies

- covariates:

  study-level covariates for meta-regression

- modelType:

  the type of meta-analysis model to fit

- outcomeType:

  the type of outcome being meta-analyzed

- priorType:

  the type of prior distribution to use

- mcmcChains:

  number of MCMC chains for posterior sampling

- mcmcIterations:

  number of MCMC iterations per chain

- warmupIterations:

  number of warmup (burn-in) iterations per chain

- publicationBias:

  whether to assess and adjust for publication bias

- posteriorPredictive:

  whether to perform posterior predictive model checking

- plotForest:

  whether to create Bayesian forest plot

- plotPosterior:

  whether to plot posterior distributions

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$modelSummary`           |     |     |     |     | a table  |
| `results$studyEffects`           |     |     |     |     | a table  |
| `results$mcmcDiagnostics`        |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$publicationBiasResults` |     |     |     |     | a table  |
| `results$forestPlot`             |     |     |     |     | an image |
| `results$posteriorPlot`          |     |     |     |     | an image |
| `results$analysisReport`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`

## Details

Key features:

- Random effects and fixed effects Bayesian meta-analysis

- Multiple outcome types (continuous, binary, time-to-event)

- Hierarchical modeling with informative and non-informative priors

- MCMC sampling with convergence diagnostics

- Posterior predictive checking and model comparison

- Meta-regression with study-level covariates

- Publication bias assessment and adjustment
