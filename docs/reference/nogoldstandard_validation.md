# nogoldstandard Diagnostic Validation Data

Dataset with 190 patients for validating a new diagnostic test against
two reference tests without a gold standard. Tests have good
characteristics (Sens: 0.88-0.82, Spec: 0.90-0.88).

## Usage

``` r
nogoldstandard_validation
```

## Format

A data frame with 190 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT190)

- New_Test:

  Factor: Test being validated ("Negative", "Positive"), Sens=0.88,
  Spec=0.90

- Reference1:

  Factor: First reference test ("Negative", "Positive"), Sens=0.85,
  Spec=0.88

- Reference2:

  Factor: Second reference test ("Negative", "Positive"), Sens=0.82,
  Spec=0.92

- test_site:

  Factor: Testing site (Academic, Community, Private)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 32% prevalence. Designed for diagnostic test validation
studies using latent class or Bayesian methods.

## Examples

``` r
data(nogoldstandard_validation)
nogoldstandard(data = nogoldstandard_validation,
               test1 = "New_Test", test1Positive = "Positive",
               test2 = "Reference1", test2Positive = "Positive",
               test3 = "Reference2", test3Positive = "Positive",
               test4Positive = "", test5Positive = "",
               clinicalPreset = "diagnostic_validation")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> 
#>  Agreement Statistics (Cohen's Kappa)                                 
#>  ──────────────────────────────────────────────────────────────────── 
#>    Test Pair                   Kappa        p-value       Agreement   
#>  ──────────────────────────────────────────────────────────────────── 
#>    New_Test vs Reference1      0.5888408    < .0000001     80.52632   
#>    New_Test vs Reference2      0.6156675    < .0000001     82.63158   
#>    Reference1 vs Reference2    0.5528360    < .0000001     78.94737   
#>  ──────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using all_positive method
#> 
#>  Tests analyzed: New_Test, Reference1, Reference2 (N=3)
#> 
#>  Disease prevalence: 23.2%
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
#>     23.15789     17.15970     29.15609   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                                   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test          Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    New_Test        100.00000    100.00000    100.00000       84.24658     79.06651     89.42664     65.67164    100.00000   
#>    Reference1      100.00000    100.00000    100.00000       76.71233     70.70243     82.72222     56.41026    100.00000   
#>    Reference2      100.00000    100.00000    100.00000       86.30137     81.41238     91.19035     68.75000    100.00000   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                                          
#>  ────────────────────────────────────────────────────────────── 
#>    Test Combination                       Count    Percentage   
#>  ────────────────────────────────────────────────────────────── 
#>    New_Test-, Reference1-, Reference2-       91      47.89474   
#>    New_Test+, Reference1+, Reference2+       44      23.15789   
#>    New_Test-, Reference1+, Reference2-       17       8.94737   
#>    New_Test+, Reference1+, Reference2-       10       5.26316   
#>    New_Test+, Reference1-, Reference2-        8       4.21053   
#>    New_Test-, Reference1-, Reference2+        8       4.21053   
#>    New_Test-, Reference1+, Reference2+        7       3.68421   
#>    New_Test+, Reference1-, Reference2+        5       2.63158   
#>  ────────────────────────────────────────────────────────────── 
#> 


```
