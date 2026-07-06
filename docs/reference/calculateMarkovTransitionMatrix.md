# Calculate Markov Transition Matrix

Creates and validates a transition probability matrix for Markov models.

## Usage

``` r
calculateMarkovTransitionMatrix(uniqueStates, transitionData, validate = TRUE)
```

## Arguments

- uniqueStates:

  Character vector of health state names

- transitionData:

  Data frame containing transition probabilities

- validate:

  Logical, whether to validate matrix properties

## Value

Matrix with transition probabilities
