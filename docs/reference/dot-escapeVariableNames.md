# Escape variable names containing special characters for formulas

Adds backticks around names that contain anything other than
`[A-Za-z0-9._]`. Centralises the helper that previously lived in
multiple `.b.R` files so the implementation stays in one place.

## Usage

``` r
.escapeVariableNames(var_names)
```

## Arguments

- var_names:

  Character vector of variable names.

## Value

Character vector with non-syntactic names backtick-quoted.
