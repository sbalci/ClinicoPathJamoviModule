# Convert Labelled Data to Factors

Handles haven::labelled vectors from SPSS/Stata/SAS files by converting
them to proper R factors with labels.

## Usage

``` r
stagemigration_convertLabelled(data, vars, verbose = FALSE)
```

## Arguments

- data:

  Data frame

- vars:

  Character vector of variable names to convert

- verbose:

  Logical, whether to print conversion messages

## Value

Data frame with labelled variables converted to factors
