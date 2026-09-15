# nogoldstandard Low Agreement Data

Dataset with 140 patients where two tests show low agreement. Tests have
moderate and different diagnostic characteristics (Sens: 0.70, 0.65;
Spec: 0.80, 0.75).

## Usage

``` r
nogoldstandard_lowagreement
```

## Format

A data frame with 140 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT140)

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.70, Spec=0.80

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.65, Spec=0.75

- age:

  Numeric: Patient age in years (mean 58, SD 14)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 30% prevalence. Tests have low correlation, representing
tests measuring different aspects of disease.

## Examples

``` r
data(nogoldstandard_lowagreement)
nogoldstandard(data = nogoldstandard_lowagreement,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "")
#> Error: Latent Class Analysis requires at least 3 tests to be statistically identifiable. Please add more tests or select a different method (e.g., Composite Reference).
```
