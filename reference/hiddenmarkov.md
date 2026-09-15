# Hidden Markov Models for Survival

Fits Hidden Markov models for survival data using the msm package.
Hidden Markov models are useful when the states are not directly
observed but are inferred from observed covariates or measurements.
Provides estimation of transition intensities, state prevalences, and
predictions with uncertainty quantification.

## Usage

``` r
hiddenmarkov(
  data,
  subject,
  state,
  time,
  covs,
  hiddenCovs,
  nstates = 3,
  qmatrix = "irreversible",
  censor,
  pci = FALSE,
  timeIntervals = "",
  obstype = "exact",
  ematrix = "identity",
  method = "BFGS",
  fixedpar = "",
  initprobs = "",
  showModel = TRUE,
  showTransitions = TRUE,
  predTimes = "1, 3, 5",
  showPrevalence = TRUE,
  showResiduals = FALSE,
  showViterbi = FALSE,
  bootstrap = FALSE,
  nboot = 500,
  plotTransitions = TRUE,
  plotPrevalence = TRUE,
  plotResiduals = FALSE,
  showEducational = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- subject:

  Subject identifier variable

- state:

  Observed state variable (can be noisy observations of true states)

- time:

  Time at which state was observed

- covs:

  Covariates to include in the transition intensity model

- hiddenCovs:

  Covariates for hidden state probabilities (misclassification model)

- nstates:

  Number of hidden states in the model

- qmatrix:

  Structure of the transition intensity matrix

- censor:

  Censoring indicator (1=observed, 0=censored)

- pci:

  Use piecewise constant transition intensities

- timeIntervals:

  Comma-separated time intervals for piecewise constant model

- obstype:

  Type of observation process

- ematrix:

  Structure of the emission/misclassification matrix

- method:

  Optimization method for parameter estimation

- fixedpar:

  Comma-separated indices of parameters to fix during estimation

- initprobs:

  Comma-separated initial state probabilities

- showModel:

  Display detailed model summary and parameter estimates

- showTransitions:

  Display transition probability matrices at specific times

- predTimes:

  Comma-separated list of time points for transition predictions

- showPrevalence:

  Display expected state prevalences over time

- showResiduals:

  Display model residuals and goodness-of-fit statistics

- showViterbi:

  Display most likely hidden state sequences (Viterbi algorithm)

- bootstrap:

  Use bootstrap methods for confidence interval estimation

- nboot:

  Number of bootstrap samples (when bootstrap is enabled)

- plotTransitions:

  Display transition intensity plots over time

- plotPrevalence:

  Display state prevalence plots over time

- plotResiduals:

  Display residual plots for model diagnostics

- showEducational:

  Display educational information about Hidden Markov models

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`      |     |     |     |     | a table  |
| `results$transitionMatrix`  |     |     |     |     | a table  |
| `results$transitionProbs`   |     |     |     |     | a table  |
| `results$statePrevalence`   |     |     |     |     | a table  |
| `results$misclassification` |     |     |     |     | a table  |
| `results$goodnessOfFit`     |     |     |     |     | a table  |
| `results$viterbiStates`     |     |     |     |     | a table  |
| `results$transitionPlot`    |     |     |     |     | an image |
| `results$prevalencePlot`    |     |     |     |     | an image |
| `results$residualPlot`      |     |     |     |     | an image |
| `results$educationalText`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
