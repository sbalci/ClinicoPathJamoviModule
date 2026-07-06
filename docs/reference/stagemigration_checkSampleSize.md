# Check Sample Size Adequacy

Validates sample size and event rates for meaningful analysis.

## Usage

``` r
stagemigration_checkSampleSize(
  n,
  n_events,
  n_predictors = 2,
  analysis_type = "standard"
)
```

## Arguments

- n:

  Total sample size

- n_events:

  Number of events

- n_predictors:

  Number of predictor variables

- analysis_type:

  Type of analysis ("basic", "standard", "comprehensive")

## Value

List with adequacy assessment and recommendations
