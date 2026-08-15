# Relative Survival Analysis

Relative survival analysis compares observed survival in a patient
cohort to expected survival in a matched general population. Essential
for cancer registry studies and population-based research.

## Usage

``` r
relativesurvival(
  data,
  time,
  status,
  age,
  sex,
  year,
  covariates = NULL,
  ratetable = "us",
  method = "poharperme",
  time_scale = "years",
  net_survival = TRUE,
  excess_mortality = TRUE,
  crude_probability = TRUE,
  age_standardized = FALSE,
  period_analysis = FALSE,
  cohort_year = "",
  regression_model = "none",
  spline_df = 4,
  plot_observed = TRUE,
  plot_expected = TRUE,
  plot_relative = TRUE,
  plot_excess = TRUE,
  confidence_level = 0.95,
  timepoints = "1,3,5,10"
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Follow-up time variable

- status:

  Event status variable

- age:

  Age variable for rate table matching

- sex:

  Sex variable for rate table matching

- year:

  Calendar year for rate table matching

- covariates:

  Covariates for regression models

- ratetable:

  Population rate table for expected survival

- method:

  Relative survival estimation method

- time_scale:

  Time unit specification

- net_survival:

  Compute net survival estimates

- excess_mortality:

  Compute excess mortality

- crude_probability:

  Compute cause-specific crude probabilities

- age_standardized:

  Perform age standardization

- period_analysis:

  Use period analysis approach

- cohort_year:

  Cohort year specification

- regression_model:

  Regression model specification

- spline_df:

  Spline complexity

- plot_observed:

  Show observed survival

- plot_expected:

  Show expected survival

- plot_relative:

  Show relative survival

- plot_excess:

  Show excess hazard

- confidence_level:

  CI level

- timepoints:

  Specific time points

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                 |     |     |     |     | a html   |
| `results$notices`              |     |     |     |     | a html   |
| `results$summary`              |     |     |     |     | a html   |
| `results$survivalTable`        |     |     |     |     | a table  |
| `results$netSurvivalTable`     |     |     |     |     | a table  |
| `results$excessMortalityTable` |     |     |     |     | a table  |
| `results$crudeProbTable`       |     |     |     |     | a table  |
| `results$regressionTable`      |     |     |     |     | a table  |
| `results$observedPlot`         |     |     |     |     | an image |
| `results$expectedPlot`         |     |     |     |     | an image |
| `results$relativePlot`         |     |     |     |     | an image |
| `results$excessPlot`           |     |     |     |     | an image |
| `results$ageStandardizedTable` |     |     |     |     | a table  |
| `results$periodAnalysisTable`  |     |     |     |     | a table  |
| `results$modelFit`             |     |     |     |     | a table  |
| `results$interpretation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$survivalTable$asDF`

`as.data.frame(results$survivalTable)`
