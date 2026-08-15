# Validate and Prepare Analysis Data

Master data validation function that performs comprehensive checks and
preparations for stage migration analysis.

## Usage

``` r
stagemigration_validateData(
  data,
  options,
  additional_vars = NULL,
  checkpoint_callback = NULL
)
```

## Arguments

- data:

  Raw input data frame

- options:

  Analysis options list containing variable names and settings

- additional_vars:

  Optional character vector of further columns the analysis needs
  (multifactorial covariates, the institution variable). They are
  checked for existence and included in the complete-case filter, so a
  covariate with missing values cannot silently drop rows later, inside
  the model fit, where the loss would go unreported.

- checkpoint_callback:

  Optional zero-argument function called between the expensive
  validation stages so jamovi can stay responsive.

## Value

List with validated data, warnings, errors, and metadata
