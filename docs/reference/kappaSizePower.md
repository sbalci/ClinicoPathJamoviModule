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

  Expected value of kappa.

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
