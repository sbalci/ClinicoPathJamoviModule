# Describe an event recode for display to the user

Turns the result of
[`.defineEventIndicator()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/dot-defineEventIndicator.md)
into the HTML disclosure block shown by every analysis that builds an
event indicator. A silent recode is a clinical-safety hazard: the reader
of a survival curve cannot otherwise see which levels were collapsed
into "censored".

## Usage

``` r
.describeEventIndicator(res, outcome_name = "outcome")
```

## Arguments

- res:

  The list returned by
  [`.defineEventIndicator()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/dot-defineEventIndicator.md).

- outcome_name:

  Display name of the outcome variable.

## Value

A character string of HTML.
