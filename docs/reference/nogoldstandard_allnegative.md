# nogoldstandard All Negative Data

Edge case dataset with 90 patients where all test results are negative.
Tests handling of degenerate scenarios.

## Usage

``` r
nogoldstandard_allnegative
```

## Format

A data frame with 90 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT090)

- Test1:

  Factor: All "Negative"

- Test2:

  Factor: All "Negative"

- age:

  Numeric: Patient age in years (mean 52, SD 11)

## Source

Generated test data for ClinicoPath package

## Details

Zero variance scenario. Should trigger appropriate error messages or
warnings about inability to estimate test characteristics.

## Examples

``` r
data(nogoldstandard_allnegative)
# Should produce error or warning
if (FALSE) { # \dontrun{
nogoldstandard(data = nogoldstandard_allnegative,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "")
} # }
```
