# Calculate Restricted Mean Survival Time for Stage Migration

RMST analysis provides an alternative to hazard ratios, reporting the
average survival time up to a specified time point.

## Usage

``` r
stagemigration_calculateRMST(
  data,
  old_stage,
  new_stage,
  time_var,
  event_var,
  tau = NULL,
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

  Binary event indicator

- tau:

  Restriction time (maximum follow-up to consider)

- checkpoint_callback:

  Optional progress callback

## Value

List with RMST results
