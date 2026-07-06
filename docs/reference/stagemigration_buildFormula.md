# Build Safe Survival Formula

Constructs survival formula with properly escaped variable names.

## Usage

``` r
stagemigration_buildFormula(
  time_var,
  event_var,
  predictors,
  interaction = FALSE
)
```

## Arguments

- time_var:

  Name of time variable

- event_var:

  Name of event variable

- predictors:

  Character vector of predictor variable names

- interaction:

  Logical, whether to include interactions

## Value

Formula object
