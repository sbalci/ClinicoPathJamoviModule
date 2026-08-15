# nogoldstandard Small Sample Data

Small dataset with only 30 patients for testing performance with limited
sample sizes. Two tests with good characteristics.

## Usage

``` r
nogoldstandard_small
```

## Format

A data frame with 30 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT030)

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.85

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.80, Spec=0.88

- age:

  Numeric: Patient age in years (mean 55, SD 10)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 30% prevalence. Small sample (n=30) tests stability of
estimation and convergence with limited data.

## Examples

``` r
data(nogoldstandard_small)
nogoldstandard(data = nogoldstandard_small,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "")
#> Error: Latent Class Analysis requires at least 3 tests to be statistically identifiable. Please add more tests or select a different method (e.g., Composite Reference).
```
