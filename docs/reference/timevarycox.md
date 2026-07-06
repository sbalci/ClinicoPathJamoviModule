# Time-Varying Covariates Cox Regression

Performs Cox proportional hazards regression with time-varying
covariates. This analysis allows covariates to change values over time
during follow-up, which is essential for modeling dynamic clinical
variables such as treatment changes, biomarker levels, or disease
progression markers.

## Usage

``` r
timevarycox(
  data,
  elapsedtime,
  timevar_data,
  outcome,
  fixed_covariates,
  subject_id,
  outcomeLevel = "1",
  robust_se = TRUE,
  cluster_se = TRUE,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_hazard_ratios = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  Time variable for survival analysis

- timevar_data:

  Variables that change over time

- outcome:

  Event indicator variable

- fixed_covariates:

  Time-constant covariates

- subject_id:

  Unique identifier for each subject

- outcomeLevel:

  Level of outcome variable indicating event

- robust_se:

  Use robust standard errors

- cluster_se:

  Cluster standard errors by subject

- show_model_summary:

  Display model summary information

- show_coefficients:

  Display regression coefficients table

- show_hazard_ratios:

  Display hazard ratios

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show methodology explanations

## Value

A results object containing:

|                             |     |     |     |     |         |
|-----------------------------|-----|-----|-----|-----|---------|
| `results$todo`              |     |     |     |     | a html  |
| `results$modelSummary`      |     |     |     |     | a html  |
| `results$coefficientsTable` |     |     |     |     | a table |
| `results$analysisSummary`   |     |     |     |     | a html  |
| `results$methodExplanation` |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficientsTable$asDF`

`as.data.frame(results$coefficientsTable)`
