# Generate Data Quality Report

Creates comprehensive data quality assessment for stage migration
analysis.

## Usage

``` r
stagemigration_dataQualityReport(
  data,
  old_stage,
  new_stage,
  time_var,
  event_var
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

  Event indicator variable name

## Value

List with quality metrics and recommendations
