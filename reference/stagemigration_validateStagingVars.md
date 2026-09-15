# Validate and Prepare Staging Variables

Comprehensive validation of staging variables including:

- Labelled data conversion

- Factor level validation

- Missing value checks

- Special character handling

## Usage

``` r
stagemigration_validateStagingVars(
  data,
  old_stage_var,
  new_stage_var,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame

- old_stage_var:

  Name of old staging variable

- new_stage_var:

  Name of new staging variable

- verbose:

  Logical, whether to print validation messages

## Value

List with validated data and metadata
