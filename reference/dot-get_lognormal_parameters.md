# Calculate log-normal distribution parameters from median survival

Calculate log-normal distribution parameters from median survival

## Usage

``` r
.get_lognormal_parameters(median_survival, sigma)
```

## Arguments

- median_survival:

  Median survival time

- sigma:

  Standard deviation of log(T)

## Value

List with mu and sigma parameters

## Details

For log-normal: log(T) ~ N(mu, sigma^2) Median: m = exp(mu) Therefore:
mu = log(m)
