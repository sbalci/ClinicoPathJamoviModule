# Calculate Advanced Concordance Metrics

Comprehensive concordance comparison with proper handling of correlated
staging systems (same patients, two measurements).

## Usage

``` r
stagemigration_calculateConcordance(
  data,
  old_stage,
  new_stage,
  time_var,
  event_var,
  perform_bootstrap = FALSE,
  bootstrap_reps = 1000,
  checkpoint_callback = NULL
)
```

## Arguments

- data:

  Data frame with complete cases

- old_stage:

  Old staging variable name

- new_stage:

  New staging variable name

- time_var:

  Survival time variable name

- event_var:

  Binary event indicator variable name

- perform_bootstrap:

  Logical, whether to perform bootstrap validation

- bootstrap_reps:

  Number of bootstrap repetitions

- checkpoint_callback:

  Optional function to call for progress updates

## Value

List with concordance metrics and comparisons
