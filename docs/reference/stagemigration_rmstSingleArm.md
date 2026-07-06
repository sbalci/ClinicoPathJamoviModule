# Single-arm restricted mean survival time (RMST)

Portable replacement for survRM2's internal (unexported) `rmst1()`.
Computes RMST for a single group up to `tau` from the Kaplan-Meier
estimator using the trapezoidal rule, with a Greenwood-based variance
approximation (the same approach used elsewhere in the module). Uses
only exported functions so it is portable and passes R CMD check.

## Usage

``` r
stagemigration_rmstSingleArm(time, status, tau)
```

## Arguments

- time:

  Numeric vector of follow-up times

- status:

  Event indicator (1 = event, 0 = censored)

- tau:

  Restriction time

## Value

List with `rmst`, `se`, `var`, and `tau`
