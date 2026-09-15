# Perform Competing Risks Analysis for Stage Migration

Analyzes stage migration accounting for competing risks using cumulative
incidence functions (CIF) and Fine-Gray models.

## Usage

``` r
stagemigration_competingRisksAnalysis(
  data,
  old_stage,
  new_stage,
  time_var,
  event_var,
  event_of_interest,
  checkpoint_callback = NULL
)
```

## Arguments

- data:

  Data frame

- old_stage:

  Old staging variable name

- new_stage:

  New staging variable name

- time_var:

  Survival time variable name

- event_var:

  Event type variable name (factor with multiple levels)

- event_of_interest:

  Primary event of interest

- checkpoint_callback:

  Optional progress callback

## Value

List with competing risks analysis results
