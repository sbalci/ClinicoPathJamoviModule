# Safe Execution with Error Handling

Standardized error handling wrapper for consistent user experience.

## Usage

``` r
stagemigration_safeExecute(
  expr,
  errorReturn = NULL,
  errorMessage = "Operation failed",
  warningMessage = NULL,
  silent = FALSE,
  context = NULL
)
```

## Arguments

- expr:

  Expression to execute

- errorReturn:

  Value to return on error

- errorMessage:

  User-friendly error message

- warningMessage:

  Optional warning to show on error

- silent:

  If TRUE, suppress error messages

- context:

  Context string for debugging

## Value

Result of expression or errorReturn on failure
