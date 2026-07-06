# Safe Atomic Conversion

Safely convert values to atomic types with fallback defaults.

## Usage

``` r
stagemigration_safeAtomic(value, type = "numeric", default = NA)
```

## Arguments

- value:

  Value to convert

- type:

  Target type ("numeric", "integer", "character", "logical")

- default:

  Default value if conversion fails

## Value

Converted value or default
