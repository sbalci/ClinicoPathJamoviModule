# Bootstrap NRI calculation with confidence intervals

Bootstrap NRI calculation with confidence intervals

## Usage

``` r
bootstrapNRI(
  new_values,
  ref_values,
  actual,
  direction = ">=",
  thresholds = NULL,
  n_boot = 1000,
  conf_level = 0.95
)
```

## Arguments

- new_values:

  Test values for new test

- ref_values:

  Test values for reference test

- actual:

  Binary outcome vector (0/1)

- direction:

  Classification direction ("\>=" or "\<=")

- thresholds:

  Risk category thresholds (NULL for continuous NRI)

- n_boot:

  Number of bootstrap iterations

- conf_level:

  Confidence level (default 0.95)

## Value

List with NRI components and confidence intervals
