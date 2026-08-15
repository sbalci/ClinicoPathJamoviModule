# Perform Monte Carlo Simulation for PSA (Deprecated)

This function is deprecated. PSA is now handled internally by
decisiongraphClass.

## Usage

``` r
performMonteCarloSimulation(
  numSimulations,
  parameters,
  baseResults,
  distributionType = "normal"
)
```

## Arguments

- numSimulations:

  Integer

- parameters:

  List

- baseResults:

  Data frame

- distributionType:

  Character string

## Value

`NULL`. This function is deprecated and performs no computation; it only
emits a deprecation warning directing callers to use the internal PSA
methods of `decisiongraphClass`.
