# Validate power calculation using simulation

Validate power calculation using simulation

## Usage

``` r
.validate_power_by_simulation(params, n_sims = 10000)
```

## Arguments

- params:

  Study parameters

- n_sims:

  Number of simulation iterations

## Value

List with simulated power and confidence interval

## Details

Performs Monte Carlo simulation to validate analytical power
calculations Useful for complex scenarios where analytical formulas may
be approximate
