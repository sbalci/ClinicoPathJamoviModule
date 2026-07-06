# Confidence Interval Approach for the Number of Subjects Required

Power Analysis for Interobserver Agreement Analysis.

## Usage

``` r
kappaSizeCI(
  outcome = "2",
  citype = "two_sided",
  kappa0 = 0.6,
  kappaL = 0.4,
  kappaU = 0.8,
  props = "0.20 , 0.80",
  raters = "2",
  alpha = 0.05
)
```

## Arguments

- outcome:

  Number of outcome level.

- citype:

  Type of confidence interval: 'two_sided' or 'one_sided'.

- kappa0:

  The null hypothesis value of kappa.

- kappaL:

  The lower limit of the kappa.

- kappaU:

  The upper limit of the kappa.

- props:

  Proportions of outcome level.

- raters:

  Number of raters.

- alpha:

  The significance level.

## Value

A results object containing:

|                        |     |     |     |     |                |
|------------------------|-----|-----|-----|-----|----------------|
| `results$text1`        |     |     |     |     | a preformatted |
| `results$text_summary` |     |     |     |     | a preformatted |
| `results$text2`        |     |     |     |     | a preformatted |
