# nogoldstandard Large Sample Data

Large dataset with 500 patients for testing computational efficiency and
performance with substantial sample sizes. Three tests with good
characteristics.

## Usage

``` r
nogoldstandard_large
```

## Format

A data frame with 500 rows and 7 variables:

- patient_id:

  Character: Patient identifier (PT0001-PT0500)

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.87, Spec=0.87

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.84, Spec=0.89

- Test3:

  Factor: Third test ("Negative", "Positive"), Sens=0.81, Spec=0.91

- age:

  Numeric: Patient age in years (mean 59, SD 13)

- sex:

  Factor: "Male" or "Female"

- study_center:

  Factor: Multi-center study (Center_1 to Center_8)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 28% prevalence. Large sample (n=500) from multi-center
study tests computational efficiency and precision of estimates.

## Examples

``` r
data(nogoldstandard_large)
nogoldstandard(data = nogoldstandard_large,
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
#>    Test1 vs Test2    0.4502935    < .0000001     76.40000   
#>    Test1 vs Test3    0.4880061    < .0000001     78.40000   
#>    Test2 vs Test3    0.4669205    < .0000001     77.60000   
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
#>  Disease prevalence: 15.0%
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
#>     15.00000     11.87019     18.12981   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test     Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1      100.00000    100.00000    100.00000       80.70588     77.24706     84.16470     47.77070    100.00000   
#>    Test2      100.00000    100.00000    100.00000       81.17647     77.75014     84.60280     48.38710    100.00000   
#>    Test3      100.00000    100.00000    100.00000       83.52941     80.27826     86.78056     51.72414    100.00000   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                             
#>  ───────────────────────────────────────────────── 
#>    Test Combination          Count    Percentage   
#>  ───────────────────────────────────────────────── 
#>    Test1-, Test2-, Test3-      256      51.20000   
#>    Test1+, Test2+, Test3+       75      15.00000   
#>    Test1-, Test2+, Test3-       39       7.80000   
#>    Test1+, Test2-, Test3-       38       7.60000   
#>    Test1-, Test2-, Test3+       29       5.80000   
#>    Test1+, Test2+, Test3-       22       4.40000   
#>    Test1+, Test2-, Test3+       22       4.40000   
#>    Test1-, Test2+, Test3+       19       3.80000   
#>  ───────────────────────────────────────────────── 
#> 


```
