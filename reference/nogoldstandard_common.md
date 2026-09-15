# nogoldstandard Common Disease Data

Dataset with 170 patients and high disease prevalence (60%). Two tests
with good characteristics (Sens: 0.85, 0.82; Spec: 0.88, 0.85).

## Usage

``` r
nogoldstandard_common
```

## Format

A data frame with 170 rows and 4 variables:

- patient_id:

  Character: Patient identifier (PT001-PT170)

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.82, Spec=0.85

- clinical_setting:

  Factor: Setting (Inpatient, Outpatient, Emergency)

## Source

Generated test data for ClinicoPath package

## Details

High prevalence (60%) typical of clinical diagnostic settings. Contrasts
with rare disease scenarios for prevalence impact assessment.

## Examples

``` r
data(nogoldstandard_common)
nogoldstandard(data = nogoldstandard_common,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "")
#> Error: Latent Class Analysis requires at least 3 tests to be statistically identifiable. Please add more tests or select a different method (e.g., Composite Reference).
```
