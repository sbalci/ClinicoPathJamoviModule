# Simulate a survival trial dataset

Simulate a survival trial dataset

## Usage

``` r
.simulate_survival_trial(
  n,
  distribution = "exponential",
  params,
  hr,
  allocation_ratio,
  accrual_period,
  follow_up_period,
  dropout_rate
)
```

## Arguments

- n:

  Total sample size

- distribution:

  Survival distribution ("exponential", "weibull", "lognormal")

- params:

  Distribution parameters

- hr:

  Hazard ratio

- allocation_ratio:

  Allocation ratio

- accrual_period:

  Accrual period (months)

- follow_up_period:

  Follow-up period (months)

- dropout_rate:

  Annual dropout rate

## Value

Data frame with simulated trial data
