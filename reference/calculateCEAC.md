# Calculate Cost-Effectiveness Acceptability Curve (CEAC)

Generates CEAC data for probabilistic sensitivity analysis.

## Usage

``` r
calculateCEAC(psaResults, thresholds, strategies)
```

## Arguments

- psaResults:

  Data frame with PSA simulation results

- thresholds:

  Numeric vector of willingness-to-pay thresholds

- strategies:

  Character vector of strategy names

## Value

Data frame with CEAC probabilities for each threshold
