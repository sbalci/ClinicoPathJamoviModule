# Convert raw test values to predicted probabilities using ROC curve

Maps raw test values to probabilities based on their position in the ROC
curve

## Usage

``` r
raw_to_prob(values, actual, direction = ">=")
```

## Arguments

- values:

  Raw test values

- actual:

  Binary outcomes (0/1)

- direction:

  Direction of test ("\>=" or "\<=")

## Value

Vector of predicted probabilities
