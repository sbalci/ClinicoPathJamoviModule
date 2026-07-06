# Calculate expected number of events for Weibull distribution

Calculate expected number of events for Weibull distribution

## Usage

``` r
.calculate_weibull_events(
  n_total,
  lambda_control,
  shape_control,
  hr,
  allocation_ratio,
  accrual_period,
  follow_up_period,
  dropout_rate
)
```

## Arguments

- n_total:

  Total sample size

- lambda_control:

  Control group scale parameter

- shape_control:

  Control group shape parameter

- hr:

  Hazard ratio

- allocation_ratio:

  Allocation ratio (control:treatment)

- accrual_period:

  Patient accrual period (months)

- follow_up_period:

  Additional follow-up period (months)

- dropout_rate:

  Annual dropout rate

## Value

List with expected events by group and total

## Details

Uses numerical integration to calculate expected events under Weibull
distribution Accounts for staggered entry, administrative censoring, and
dropout
