# Bootstrap IDI calculation with confidence intervals

Calculates Integrated Discrimination Improvement with bootstrap
confidence intervals

## Usage

``` r
bootstrapIDI(
  new_values,
  ref_values,
  actual,
  direction = ">=",
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

- n_boot:

  Number of bootstrap iterations

- conf_level:

  Confidence level (default 0.95)

## Value

List with IDI, confidence intervals, and p-value
