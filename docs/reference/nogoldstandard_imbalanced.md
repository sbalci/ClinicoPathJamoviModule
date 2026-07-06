# nogoldstandard Imbalanced Test Characteristics Data

Dataset with 160 patients featuring two tests with complementary
characteristics: one very sensitive (0.95) but less specific (0.70), one
very specific (0.95) but less sensitive (0.70).

## Usage

``` r
nogoldstandard_imbalanced
```

## Format

A data frame with 160 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT160)

- Sensitive_Test:

  Factor: High sensitivity test ("Negative", "Positive"), Sens=0.95,
  Spec=0.70

- Specific_Test:

  Factor: High specificity test ("Negative", "Positive"), Sens=0.70,
  Spec=0.95

- age:

  Numeric: Patient age in years (mean 57, SD 13)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 28% prevalence. Demonstrates complementary test
characteristics and value of combining tests with different strengths.

## Examples

``` r
data(nogoldstandard_imbalanced)
nogoldstandard(data = nogoldstandard_imbalanced,
               test1 = "Sensitive_Test", test1Positive = "Positive",
               test2 = "Specific_Test", test2Positive = "Positive",
               test3Positive = "", test4Positive = "",
               test5Positive = "", method = "composite")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> WARNING: Composite Ties
#> Composite reference with even number of tests may result in ties. Consider using an odd number of tests or a different method.
#>  Agreement Statistics (Cohen's Kappa)                                       
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Test Pair                          Kappa        p-value      Agreement   
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Sensitive_Test vs Specific_Test    0.3778354    0.0000005     70.00000   
#>  ────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using composite method
#> 
#>  Tests analyzed: Sensitive_Test, Specific_Test (N=2)
#> 
#>  Disease prevalence: 51.2%
#> 
#>  Test sensitivities: Range from 51.2% to 90.2%
#> 
#>  Clinical interpretation: High prevalence setting - high PPV expected,
#>  focus on confirming disease
#> 
#>  <div style='background: #f8f9fa; padding: 20px; border-radius: 8px;
#>  margin: 15px 0; border-left: 4px solid #007bff;'><h3 style='color:
#>  #007bff; margin-top: 0;'> Method Selection Guide
#> 
#>  <div style='margin: 15px 0; padding: 15px; background: #e8f5e8;
#>  border-radius: 5px;'><h4 style='color: #2e7d32; margin-top: 0;'>
#>  Latent Class Analysis (Recommended)
#> 
#>  Description: Most robust method using mixture models. Estimates
#>  disease prevalence and test parameters simultaneously.
#> 
#>  Best for: Diagnostic validation studies with 3+ tests and N>=100
#> 
#>  Strengths: Handles conditional dependence, provides model fit
#>  statistics, most statistically rigorous
#> 
#>  <div style='margin: 15px 0; padding: 15px; background: #e3f2fd;
#>  border-radius: 5px;'><h4 style='color: #1565c0; margin-top: 0;'>
#>  Bayesian Analysis
#> 
#>  Description: Incorporates prior knowledge about test performance using
#>  Bayesian methods.
#> 
#>  Best for: Studies where you have prior information about expected
#>  sensitivity/specificity
#> 
#>  Strengths: Uses prior knowledge, handles uncertainty well, good for
#>  smaller samples
#> 
#>  <div style='margin: 15px 0; padding: 15px; background: #fff3e0;
#>  border-radius: 5px;'><h4 style='color: #ef6c00; margin-top: 0;'>
#>  Composite Reference
#> 
#>  Description: Uses majority vote of available tests as pseudo-gold
#>  standard.
#> 
#>  Best for: Inter-rater agreement studies with 3+ tests, exploratory
#>  analysis
#> 
#>  Strengths: Simple and intuitive, requires minimal assumptions, good
#>  starting point
#> 
#>  <div style='margin: 15px 0; padding: 15px; background: #fce4ec;
#>  border-radius: 5px;'><h4 style='color: #c2185b; margin-top: 0;'> All
#>  Tests Positive
#> 
#>  Description: Conservative approach - disease present only if ALL tests
#>  are positive.
#> 
#>  Best for: Highly specific diagnoses where false positives are very
#>  costly
#> 
#>  Strengths: High specificity reference, minimizes false positives
#> 
#>  <div style='margin: 15px 0; padding: 15px; background: #e8f5e8;
#>  border-radius: 5px;'><h4 style='color: #388e3c; margin-top: 0;'> Any
#>  Test Positive
#> 
#>  Description: Liberal approach - disease present if ANY test is
#>  positive.
#> 
#>  Best for: Population screening scenarios where missing cases is costly
#> 
#>  Strengths: High sensitivity reference, minimizes false negatives
#> 
#>  <div style='margin: 15px 0; padding: 10px; background: #fff8e1;
#>  border-radius: 5px; border-left: 3px solid #ffb300;'><h4 style='color:
#>  #e65100; margin-top: 0;'> Selection Tips
#> 
#>  Start with Latent Class Analysis for most diagnostic studiesUse
#>  Composite Reference for quick exploratory analysisChoose All/Any Tests
#>  Positive based on clinical consequences of errorsConsider Bayesian if
#>  you have strong prior information
#> 
#>  Disease Prevalence                      
#>  ─────────────────────────────────────── 
#>    Estimate     Lower CI     Upper CI    
#>  ─────────────────────────────────────── 
#>     51.25000     43.50498     58.99502   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                                       
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test              Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Sensitive_Test       90.24390     85.64626     94.84154      100.00000    100.00000    100.00000    100.00000     90.69767   
#>    Specific_Test        51.21951     43.47438     58.96465      100.00000    100.00000    100.00000    100.00000     66.10169   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                                      
#>  ────────────────────────────────────────────────────────── 
#>    Test Combination                   Count    Percentage   
#>  ────────────────────────────────────────────────────────── 
#>    Sensitive_Test-, Specific_Test-       78      48.75000   
#>    Sensitive_Test+, Specific_Test-       40      25.00000   
#>    Sensitive_Test+, Specific_Test+       34      21.25000   
#>    Sensitive_Test-, Specific_Test+        8       5.00000   
#>  ────────────────────────────────────────────────────────── 
#> 


```
