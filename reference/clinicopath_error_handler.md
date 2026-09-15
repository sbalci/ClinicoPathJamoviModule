# ClinicoPath Error Handler

Enhanced error handler with clinical context

## Usage

``` r
clinicopath_error_handler(
  error,
  function_name = "unknown",
  clinical_context = ""
)
```

## Arguments

- error:

  The error object

- function_name:

  Name of the function where error occurred

- clinical_context:

  Clinical context for the error

## Value

A list with `success` (FALSE), `error` (TRUE), a user-friendly
`error_message`, an `error_id`, and the `detailed_log` entry.
