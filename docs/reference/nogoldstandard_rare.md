# nogoldstandard Rare Disease Data

Dataset with 300 patients and very low disease prevalence (5%). Three
tests with good characteristics (Sens: 0.80-0.75, Spec: 0.90-0.88).

## Usage

``` r
nogoldstandard_rare
```

## Format

A data frame with 300 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT300)

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.80, Spec=0.90

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.75, Spec=0.92

- Test3:

  Factor: Third test ("Negative", "Positive"), Sens=0.78, Spec=0.88

- screening_site:

  Factor: Screening site (Site_1 to Site_10)

## Source

Generated test data for ClinicoPath package

## Details

Rare disease setting (5% prevalence) typical of population screening.
Tests stability of estimation with few positive cases.

## Examples

``` r
data(nogoldstandard_rare)
nogoldstandard(data = nogoldstandard_rare,
               test1 = "Test1", test1Positive = "Positive",
               test2 = "Test2", test2Positive = "Positive",
               test3 = "Test3", test3Positive = "Positive",
               test4Positive = "", test5Positive = "")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> 
#>  Agreement Statistics (Cohen's Kappa)                      
#>  ───────────────────────────────────────────────────────── 
#>    Test Pair         Kappa        p-value      Agreement   
#>  ───────────────────────────────────────────────────────── 
#>    Test1 vs Test2    0.1767473    0.1003799     83.66667   
#>    Test1 vs Test3    0.1067762    0.3107576     80.66667   
#>    Test2 vs Test3    0.2679128    0.0062952     84.33333   
#>  ───────────────────────────────────────────────────────── 
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
#>  Disease prevalence: 1.3%
#> 
#>  Test sensitivities: Range from 100.0% to 100.0%
#> 
#>  Clinical interpretation: Low prevalence setting - high NPV expected,
#>  focus on ruling out disease
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
#>      1.33333    3.543086e-4      2.63124   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test     Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1      100.00000    100.00000    100.00000       89.86486     86.44981     93.27992     11.76471    100.00000   
#>    Test2      100.00000    100.00000    100.00000       90.20270     86.83875     93.56666     12.12121    100.00000   
#>    Test3      100.00000    100.00000    100.00000       87.83784     84.13927     91.53641     10.00000    100.00000   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                             
#>  ───────────────────────────────────────────────── 
#>    Test Combination          Count    Percentage   
#>  ───────────────────────────────────────────────── 
#>    Test1-, Test2-, Test3-      219      73.00000   
#>    Test1-, Test2-, Test3+       23       7.66667   
#>    Test1+, Test2-, Test3-       21       7.00000   
#>    Test1-, Test2+, Test3-       15       5.00000   
#>    Test1-, Test2+, Test3+        9       3.00000   
#>    Test1+, Test2+, Test3-        5       1.66667   
#>    Test1+, Test2-, Test3+        4       1.33333   
#>    Test1+, Test2+, Test3+        4       1.33333   
#>  ───────────────────────────────────────────────── 
#> 


```
