# ClinicoPath Warning Handler

Enhanced warning handler with clinical context

## Usage

``` r
clinicopath_warning_handler(
  warning,
  function_name = "unknown",
  clinical_context = ""
)
```

## Arguments

- warning:

  The warning object

- function_name:

  Name of the function where warning occurred

- clinical_context:

  Clinical context for the warning

## Value

A list with `success` (TRUE), `warning` (TRUE), a user-friendly
`warning_message`, a `warning_id`, and the `detailed_log` entry.
