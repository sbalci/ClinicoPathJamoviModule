# Lowest Expected Value for a fixed sample size

Lowest Expected Value for a fixed sample size.

## Usage

``` r
kappaSizeFixedN(
  outcome = "2",
  kappa0 = 0.4,
  props = "0.20 , 0.80",
  raters = "2",
  alpha = 0.05,
  n = 100
)
```

## Arguments

- outcome:

  Number of outcome level.

- kappa0:

  Expected value of kappa.

- props:

  Proportions of outcome level.

- raters:

  Number of raters.

- alpha:

  Significance level.

- n:

  Sample size.

## Value

A results object containing:

|                        |     |     |     |     |                |
|------------------------|-----|-----|-----|-----|----------------|
| `results$text1`        |     |     |     |     | a preformatted |
| `results$text_summary` |     |     |     |     | a preformatted |
| `results$text2`        |     |     |     |     | a preformatted |
