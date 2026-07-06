# Escape Variable Names for Safe Handling

Ensures variable names are safe for use in formulas and data operations.
Handles spaces, special characters, and reserved words.

## Usage

``` r
stagemigration_escapeVar(varname, backticks = TRUE)
```

## Arguments

- varname:

  Character string or vector of variable names

- backticks:

  Logical, whether to wrap in backticks (default TRUE)

## Value

Escaped variable name(s)
