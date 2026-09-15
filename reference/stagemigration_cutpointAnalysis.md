# Optimal Cutpoint Analysis for Stage Grouping

Identifies optimal cutpoints for creating stage groups based on survival
discrimination.

## Usage

``` r
stagemigration_cutpointAnalysis(
  data,
  stage_var,
  time_var,
  event_var,
  method = "maxstat"
)
```

## Arguments

- data:

  Data frame

- stage_var:

  Staging variable (continuous or ordinal)

- time_var:

  Survival time variable

- event_var:

  Binary event indicator

- method:

  Method for cutpoint selection ("maxstat", "median", "tertiles")

## Value

List with cutpoint analysis results
