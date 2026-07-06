# nogoldstandard Pathology Data - Inter-Pathologist Agreement

Three-pathologist dataset with 180 patients for assessing diagnostic
agreement without a gold standard. Pathologists have varying sensitivity
(0.88, 0.85, 0.82) and high specificity (0.92, 0.90, 0.93).

## Usage

``` r
nogoldstandard_pathology
```

## Format

A data frame with 180 rows and 6 variables:

- patient_id:

  Character: Patient identifier (PT001-PT180)

- Pathologist1:

  Factor: First pathologist diagnosis ("Benign", "Malignant"),
  Sens=0.88, Spec=0.92

- Pathologist2:

  Factor: Second pathologist diagnosis ("Benign", "Malignant"),
  Sens=0.85, Spec=0.90

- Pathologist3:

  Factor: Third pathologist diagnosis ("Benign", "Malignant"),
  Sens=0.82, Spec=0.93

- tumor_site:

  Factor: Tumor location (Lung, Breast, Colon, Prostate)

- specimen_quality:

  Factor: Specimen quality (Adequate, Limited, Poor)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 25% malignancy prevalence. Pathologists show realistic
variation in diagnostic accuracy. Ideal for pathology agreement studies
using latent class analysis.

## Examples

``` r
data(nogoldstandard_pathology)
nogoldstandard(data = nogoldstandard_pathology,
               test1 = "Pathologist1", test1Positive = "Malignant",
               test2 = "Pathologist2", test2Positive = "Malignant",
               test3 = "Pathologist3", test3Positive = "Malignant",
               test4Positive = "", test5Positive = "",
               clinicalPreset = "pathology_agreement")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> 
#>  Agreement Statistics (Cohen's Kappa)                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Test Pair                       Kappa        p-value       Agreement   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Pathologist1 vs Pathologist2    0.4633028    < .0000001     78.33333   
#>    Pathologist1 vs Pathologist3    0.6025237    < .0000001     84.44444   
#>    Pathologist2 vs Pathologist3    0.5065789    < .0000001     80.55556   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using all_positive method
#> 
#>  Tests analyzed: Pathologist1, Pathologist2, Pathologist3 (N=3)
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
#>     13.33333      8.36733     18.29934   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                                     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test            Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Pathologist1      100.00000    100.00000    100.00000       83.33333     77.88899     88.77768     48.00000    100.00000   
#>    Pathologist2      100.00000    100.00000    100.00000       82.69231     77.16563     88.21898     47.05882    100.00000   
#>    Pathologist3      100.00000    100.00000    100.00000       85.89744     80.81290     90.98197     52.17391    100.00000   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                                                  
#>  ────────────────────────────────────────────────────────────────────── 
#>    Test Combination                               Count    Percentage   
#>  ────────────────────────────────────────────────────────────────────── 
#>    Pathologist1-, Pathologist2-, Pathologist3-      105      58.33333   
#>    Pathologist1+, Pathologist2+, Pathologist3+       24      13.33333   
#>    Pathologist1-, Pathologist2+, Pathologist3-       13       7.22222   
#>    Pathologist1+, Pathologist2-, Pathologist3+       10       5.55556   
#>    Pathologist1+, Pathologist2-, Pathologist3-        9       5.00000   
#>    Pathologist1+, Pathologist2+, Pathologist3-        7       3.88889   
#>    Pathologist1-, Pathologist2+, Pathologist3+        7       3.88889   
#>    Pathologist1-, Pathologist2-, Pathologist3+        5       2.77778   
#>  ────────────────────────────────────────────────────────────────────── 
#> 


```
