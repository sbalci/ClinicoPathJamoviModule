# nogoldstandard Missing Data

Dataset with 150 patients including missing values in test results
(~5-8% missingness per test). Three tests with good characteristics.

## Usage

``` r
nogoldstandard_missing
```

## Format

A data frame with 150 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT150)

- Test1:

  Factor: First test ("Negative", "Positive"), ~7% missing, Sens=0.85,
  Spec=0.85

- Test2:

  Factor: Second test ("Negative", "Positive"), ~5% missing, Sens=0.80,
  Spec=0.88

- Test3:

  Factor: Third test ("Negative", "Positive"), ~8% missing, Sens=0.82,
  Spec=0.90

- age:

  Numeric: Patient age in years (mean 58, SD 12)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 30% prevalence. Missing data introduced randomly to test
listwise deletion and missing data handling.

## Examples

``` r
data(nogoldstandard_missing)
nogoldstandard(data = nogoldstandard_missing,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3 = "Test3", test3Positive = "Positive",
               test4Positive = "", test5Positive = "")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> 
#>  Agreement Statistics (Cohen's Kappa)                       
#>  ────────────────────────────────────────────────────────── 
#>    Test Pair         Kappa        p-value       Agreement   
#>  ────────────────────────────────────────────────────────── 
#>    Test1 vs Test2    0.5472441    < .0000001     80.83333   
#>    Test1 vs Test3    0.4512195     0.0000012     77.50000   
#>    Test2 vs Test3    0.3957704     0.0000343     75.00000   
#>  ────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using all_positive method
#> 
#>  Tests analyzed: Test1, Test2, Test3 (N=3)
#> 
#>  Disease prevalence: 13.3%
#> 
#>  Test sensitivities: Range from 100.0% to 100.0%
#> 
#>  Clinical interpretation: Moderate prevalence setting - balanced
#>  diagnostic performance
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
#>     13.33333      7.25124     19.41542   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test     Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1      100.00000    100.00000    100.00000       80.76923     73.71778     87.82069     44.44444    100.00000   
#>    Test2      100.00000    100.00000    100.00000       79.80769     72.62524     86.99015     43.24324    100.00000   
#>    Test3      100.00000    100.00000    100.00000       83.65385     77.03765     90.27004     48.48485    100.00000   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                             
#>  ───────────────────────────────────────────────── 
#>    Test Combination          Count    Percentage   
#>  ───────────────────────────────────────────────── 
#>    Test1-, Test2-, Test3-       64      53.33333   
#>    Test1+, Test2+, Test3+       16      13.33333   
#>    Test1+, Test2+, Test3-        9       7.50000   
#>    Test1-, Test2+, Test3-        8       6.66667   
#>    Test1-, Test2-, Test3+        8       6.66667   
#>    Test1+, Test2-, Test3-        6       5.00000   
#>    Test1+, Test2-, Test3+        5       4.16667   
#>    Test1-, Test2+, Test3+        4       3.33333   
#>  ───────────────────────────────────────────────── 
#> 


```
