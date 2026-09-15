# nogoldstandard Perfect Agreement Data

Edge case dataset with 100 patients where two tests are identical (100%
agreement). Test characteristics: Sens=0.85, Spec=0.85.

## Usage

``` r
nogoldstandard_perfect
```

## Format

A data frame with 100 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT100)

- Test1:

  Factor: First test ("Negative", "Positive")

- Test2:

  Factor: Second test ("Negative", "Positive"), identical to Test1

- age:

  Numeric: Patient age in years (mean 55, SD 12)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 30% prevalence. Perfect agreement violates conditional
independence assumption of latent class models. Tests handling of
degenerate cases.

## Examples

``` r
data(nogoldstandard_perfect)
nogoldstandard(data = nogoldstandard_perfect,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "")
#> Error: Latent Class Analysis requires at least 3 tests to be statistically identifiable. Please add more tests or select a different method (e.g., Composite Reference).
```
