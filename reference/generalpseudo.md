# Generalized Pseudo-Observations

Generalized pseudo-observations approach for survival analysis,
competing risks, and other censored data problems. This method provides
a flexible framework for analyzing survival outcomes using standard
statistical methods by transforming survival data into
pseudo-observations that can be analyzed with conventional regression
techniques. Supports various functionals including survival
probabilities, cumulative incidence functions, and restricted mean
survival times.

## Usage

``` r
generalpseudo(
  data,
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  functional_type = "survival",
  cause_specific = 1,
  time_points = "12,24,60",
  quantile_prob = 0.5,
  rmst_tau = 60,
  pseudo_method = "jackknife",
  regression_type = "linear",
  confidence_level = 0.95,
  bootstrap_reps = 500,
  clustering_var,
  show_pseudo_values = FALSE,
  show_model_diagnostics = TRUE,
  show_comparison = FALSE,
  functional_plot = TRUE,
  residual_plot = FALSE,
  pseudo_plot = FALSE,
  showSummaries = TRUE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Time to event or censoring

- outcome:

  Event indicator (for survival: 1 = event, 0 = censored; for competing
  risks: 0 = censored, 1,2,... = specific causes)

- explanatory:

  Explanatory variables for pseudo-observation regression modeling

- outcomeLevel:

  Level indicating event occurrence (for survival analysis)

- functional_type:

  Type of functional for pseudo-observation calculation

- cause_specific:

  Cause of interest for competing risks analysis (competing events coded
  as different integers)

- time_points:

  Comma-separated list of time points at which to calculate
  pseudo-observations

- quantile_prob:

  Probability for quantile pseudo-observations (e.g., 0.5 for median
  survival)

- rmst_tau:

  Restriction time tau for restricted mean survival time calculations

- pseudo_method:

  Method for calculating pseudo-observations

- regression_type:

  Type of regression model for pseudo-observations

- confidence_level:

  Confidence level for confidence intervals

- bootstrap_reps:

  Number of bootstrap replications for pseudo-observation calculation

- clustering_var:

  Variable for clustered data analysis (optional)

- show_pseudo_values:

  Display calculated pseudo-observation values

- show_model_diagnostics:

  Display comprehensive model diagnostic information

- show_comparison:

  Compare pseudo-observation results with traditional methods

- functional_plot:

  Display plots of the estimated functionals

- residual_plot:

  Display residual diagnostic plots

- pseudo_plot:

  Display pseudo-observation distribution plots

- showSummaries:

  Show comprehensive analysis summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                   |     |     |     |     | a html   |
| `results$functionalSummary`      |     |     |     |     | a table  |
| `results$regressionResults`      |     |     |     |     | a table  |
| `results$pseudoValues`           |     |     |     |     | a table  |
| `results$modelDiagnostics`       |     |     |     |     | a table  |
| `results$methodComparison`       |     |     |     |     | a table  |
| `results$competingRisks`         |     |     |     |     | a table  |
| `results$restrictedMean`         |     |     |     |     | a table  |
| `results$quantileResults`        |     |     |     |     | a table  |
| `results$methodologyExplanation` |     |     |     |     | a html   |
| `results$analysisSummary`        |     |     |     |     | a html   |
| `results$functionalPlot`         |     |     |     |     | an image |
| `results$residualPlot`           |     |     |     |     | an image |
| `results$pseudoPlot`             |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$functionalSummary$asDF`

`as.data.frame(results$functionalSummary)`
