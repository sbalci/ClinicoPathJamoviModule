# Calculate hazard function for Weibull distribution

Calculate hazard function for Weibull distribution

## Usage

``` r
.weibull_hazard(t, lambda, shape)
```

## Arguments

- t:

  Time point

- lambda:

  Scale parameter

- shape:

  Shape parameter

## Value

Hazard h(t)

## Details

h(t) = lambda \* shape \* (lambda \* t)^(shape - 1)
