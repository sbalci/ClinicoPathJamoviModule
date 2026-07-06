# nogoldstandard Tumor Marker Data - Four-Marker Panel

Four tumor marker dataset with 220 patients for evaluating marker panel
performance without gold standard. Markers: CA125, HE4, CEA, AFP with
varying sensitivity (0.75-0.68) and specificity (0.88-0.85).

## Usage

``` r
nogoldstandard_tumormarker
```

## Format

A data frame with 220 rows and 7 variables:

- patient_id:

  Character: Patient identifier (PT001-PT220)

- CA125:

  Factor: CA125 level ("Normal", "Elevated"), Sens=0.75, Spec=0.88

- HE4:

  Factor: HE4 level ("Normal", "Elevated"), Sens=0.70, Spec=0.85

- CEA:

  Factor: CEA level ("Normal", "Elevated"), Sens=0.68, Spec=0.90

- AFP:

  Factor: AFP level ("Normal", "Elevated"), Sens=0.72, Spec=0.87

- age:

  Numeric: Patient age in years (mean 62, SD 10)

- risk_category:

  Factor: Risk level (Low, Moderate, High)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 20% cancer prevalence (screening context). Multiple
markers enable composite reference and latent class analysis
comparisons.

## Examples

``` r
data(nogoldstandard_tumormarker)
nogoldstandard(data = nogoldstandard_tumormarker,
               test1 = "CA125", test1Positive = "Elevated",
               test2 = "HE4", test2Positive = "Elevated",
               test3 = "CEA", test3Positive = "Elevated",
               test4 = "AFP", test4Positive = "Elevated",
               test5Positive = "",
               clinicalPreset = "tumor_markers")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> 
#>  Agreement Statistics (Cohen's Kappa)                    
#>  ─────────────────────────────────────────────────────── 
#>    Test Pair       Kappa        p-value      Agreement   
#>  ─────────────────────────────────────────────────────── 
#>    CA125 vs HE4    0.2513206    0.0009848     69.54545   
#>    CA125 vs CEA    0.3145332    0.0000379     73.18182   
#>    CA125 vs AFP    0.3407152    0.0000035     73.18182   
#>    HE4 vs CEA      0.2052699    0.0103947     69.09091   
#>    HE4 vs AFP      0.2364230    0.0021283     69.09091   
#>    CEA vs AFP      0.3221419    0.0000247     73.63636   
#>  ─────────────────────────────────────────────────────── 
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using all_positive method
#> 
#>  Tests analyzed: CA125, HE4, CEA, AFP (N=4)
#> 
#>  Disease prevalence: 5.5%
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
#>      5.45455      2.45375      8.45534   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test     Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    CA125      100.00000    100.00000    100.00000       75.48077     69.79606     81.16548     19.04762    100.00000   
#>    HE4        100.00000    100.00000    100.00000       75.96154     70.31494     81.60814     19.35484    100.00000   
#>    CEA        100.00000    100.00000    100.00000       79.80769     74.50310     85.11229     22.22222    100.00000   
#>    AFP        100.00000    100.00000    100.00000       75.96154     70.31494     81.60814     19.35484    100.00000   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test Cross-Tabulation                               
#>  ─────────────────────────────────────────────────── 
#>    Test Combination            Count    Percentage   
#>  ─────────────────────────────────────────────────── 
#>    CA125-, HE4-, CEA-, AFP-       90      40.90909   
#>    CA125-, HE4+, CEA-, AFP-       21       9.54545   
#>    CA125+, HE4-, CEA-, AFP-       16       7.27273   
#>    CA125-, HE4-, CEA-, AFP+       16       7.27273   
#>    CA125-, HE4-, CEA+, AFP-       15       6.81818   
#>    CA125+, HE4+, CEA+, AFP+       12       5.45455   
#>    CA125+, HE4-, CEA+, AFP+        9       4.09091   
#>    CA125+, HE4+, CEA-, AFP-        6       2.72727   
#>    CA125+, HE4-, CEA-, AFP+        6       2.72727   
#>    CA125+, HE4+, CEA-, AFP+        6       2.72727   
#>    CA125+, HE4+, CEA+, AFP-        5       2.27273   
#>    CA125-, HE4+, CEA-, AFP+        5       2.27273   
#>    CA125-, HE4+, CEA+, AFP+        5       2.27273   
#>    CA125+, HE4-, CEA+, AFP-        3       1.36364   
#>    CA125-, HE4-, CEA+, AFP+        3       1.36364   
#>    CA125-, HE4+, CEA+, AFP-        2       0.90909   
#>  ─────────────────────────────────────────────────── 
#> 


```
