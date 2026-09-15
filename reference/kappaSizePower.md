# Power Approach for the Number of Subjects Required

Power Analysis for Interobserver Agreement Analysis.

## Usage

``` r
kappaSizePower(
  outcome = "2",
  kappa0 = 0.4,
  kappa1 = 0.6,
  props = "0.20 , 0.80",
  raters = "2",
  alpha = 0.05,
  power = 0.8
)
```

## Arguments

- outcome:

  Number of outcome level.

- kappa0:

  The null hypothesis value of kappa - the level of agreement the study
  tests against, versus the alternative kappa1. kappaSize documents this
  argument as "the null hypothesis for the kappa hypothesis test". It is
  NOT the anticipated value; kappaSizeCI and kappaSizeFixedN use kappa0
  for the anticipated value, which is a different quantity.

- kappa1:

  Expected value of kappa.

- props:

  Proportions of outcome level.

- raters:

  Number of raters.

- alpha:

  Significance level.

- power:

  Power.

## Value

A results object containing:

|                        |     |     |     |     |                |
|------------------------|-----|-----|-----|-----|----------------|
| `results$text1`        |     |     |     |     | a preformatted |
| `results$text_summary` |     |     |     |     | a preformatted |
| `results$text2`        |     |     |     |     | a preformatted |
