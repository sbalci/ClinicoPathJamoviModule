# nogoldstandard All Positive Data

Edge case dataset with 80 patients where all test results are positive.
Tests handling of degenerate scenarios.

## Usage

``` r
nogoldstandard_allpositive
```

## Format

A data frame with 80 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT080)

- Test1:

  Factor: All "Positive"

- Test2:

  Factor: All "Positive"

- age:

  Numeric: Patient age in years (mean 55, SD 10)

## Source

Generated test data for ClinicoPath package

## Details

Zero variance scenario. Should trigger appropriate error messages or
warnings about inability to estimate test characteristics.

## Examples

``` r
data(nogoldstandard_allpositive)
# Should produce error or warning
if (FALSE) { # \dontrun{
nogoldstandard(data = nogoldstandard_allpositive,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "")
} # }
```
