# G-computation (Parametric G-formula)

Estimates the marginal (population-average) causal effect of a binary
point treatment using the parametric g-formula (g-computation). An
outcome model is fitted conditional on treatment and covariates; the
fitted model then predicts each subject's outcome under treatment and
under no treatment, and these predictions are averaged (standardized)
over the covariate distribution to give the counterfactual means E(Y^1)
and E(Y^0). The average treatment effect is reported as a difference
(and, for binary outcomes, a risk ratio), with percentile bootstrap
confidence intervals. This standardization removes confounding by the
measured covariates without requiring a propensity model.

## Usage

``` r
gcomputation(
  data,
  outcome,
  outcomeType = "continuous",
  outcomeEvent,
  treatment,
  treatmentLevel,
  covariates,
  interactions = FALSE,
  bootstrap_n = 1000,
  conf_level = 0.95,
  showCounterfactual = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per subject).

- outcome:

  The outcome. Continuous, or a two-level factor / 0-1 numeric for
  binary.

- outcomeType:

  Whether the outcome is continuous (linear model) or binary (logistic
  model).

- outcomeEvent:

  For a binary factor outcome, the level treated as the event.

- treatment:

  The binary treatment / exposure variable.

- treatmentLevel:

  The level of the treatment variable representing "treated".

- covariates:

  Covariates to adjust for (measured confounders).

- interactions:

  Include treatment-by-covariate interactions in the outcome model,
  allowing effect modification. The marginal effect is still
  standardized over the covariate distribution.

- bootstrap_n:

  Number of bootstrap resamples for the confidence interval.

- conf_level:

  Confidence level for the bootstrap interval.

- showCounterfactual:

  Report the standardized counterfactual means E(Y^1) and E(Y^0).

- showPlot:

  Display the counterfactual means with the treatment effect.

- showSummary:

  Display a plain-language summary of the estimated effect.

- showExplanation:

  Display an explanation of the g-formula.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$mainTable`           |     |     |     |     | a table  |
| `results$counterfactualTable` |     |     |     |     | a table  |
| `results$plot`                |     |     |     |     | an image |
| `results$summary`             |     |     |     |     | a html   |
| `results$explanation`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$mainTable$asDF`

`as.data.frame(results$mainTable)`

## Examples

``` r
# \donttest{
gcomputation(
    data = mydata,
    outcome = "death",
    treatment = "treated",
    covariates = vars(age, stage, grade),
    outcomeType = "binary")
#> Error in gcomputation(data = mydata, outcome = "death", treatment = "treated",     covariates = vars(age, stage, grade), outcomeType = "binary"): argument "outcomeEvent" is missing, with no default
# }
```
