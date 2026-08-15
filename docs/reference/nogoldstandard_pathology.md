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
#> WARNING: Clinical preset: pathology agreement
#> Optimal for assessing agreement between pathologists or observers Use for inter-rater reliability studies in pathology This preset does NOT change your settings automatically -- set them yourself in the options panel: Analysis method: currently "latent_class", recommended "composite"; Bootstrap samples: currently 1000, recommended 500.
#> 
#> Analysing 180 cases
#> All 180 cases have a result for every selected test.
#>  Agreement Statistics (Cohen's Kappa)                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Test Pair                       Kappa        p-value       Agreement   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Pathologist1 vs Pathologist2    0.4633028    < .0000001     78.33333   
#>    Pathologist1 vs Pathologist3    0.6025237    < .0000001     84.44444   
#>    Pathologist2 vs Pathologist3    0.5065789    < .0000001     80.55556   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. Kappa standard errors and p-values use a large-sample normal
#>    approximation rather than the exact asymptotic SE (e.g.
#>    vcd::Kappa); interpret p-values cautiously, especially in small
#>    samples.
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using latent_class method
#> 
#>  Tests analyzed: Pathologist1, Pathologist2, Pathologist3 (N=3)
#> 
#>  Disease prevalence: 30.4%
#> 
#>  Test sensitivities: Range from 70.9% to 79.2%
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
#>  Strengths: The only method here that estimates accuracy rather than
#>  agreement with a self-built reference; provides model fit statistics.
#>  Assumes the tests are conditionally independent given true status --
#>  it does NOT model conditional dependence
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
#>  Strengths: Simple and intuitive. Not an accuracy estimate: each test
#>  helps build the standard it is judged against, which inflates its
#>  apparent performance. Needs 3+ tests -- with 2 a tie counts as
#>  diseased, making it identical to Any Test Positive
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
#>  Strengths: A deliberately strict reference. Sensitivity and NPV cannot
#>  be estimated under this rule -- they are fixed at 100% by construction
#>  -- so only specificity and PPV are shown, and both are inflated by the
#>  same circularity
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
#>  Strengths: A deliberately permissive reference. Specificity and PPV
#>  cannot be estimated under this rule -- they are fixed at 100% by
#>  construction -- so only sensitivity and NPV are shown, and both are
#>  inflated by the same circularity
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
#>     30.44824     23.72549     37.17099   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                                     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test            Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Pathologist1       78.01015     67.04492     88.97537       94.21285     90.12265     98.30306     85.50978     90.72928   
#>    Pathologist2       70.85991     58.82961     82.89021       90.28387     85.09576     95.47198     76.14915     87.61956   
#>    Pathologist3       79.16987     68.41868     89.92106       97.91562     95.41313    100.00000     94.32716     91.48035   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. 95% intervals are normal-approximation (Wald) intervals, using the estimated number of diseased cases as the
#>    denominator for sensitivity and non-diseased for specificity. They treat the estimates as observed proportions and so
#>    understate the uncertainty of a latent-variable model; enable Bootstrap for intervals that account for the estimation
#>    itself.
#> 
#> 
#>  Model Fit Statistics                  
#>  ───────────────────────────────────── 
#>    Statistic             Value         
#>  ───────────────────────────────────── 
#>    BIC                    553.066616   
#>    AIC                    530.715918   
#>    Log-Likelihood        -258.357959   
#>    Degrees of Freedom       0.000000   
#>  ───────────────────────────────────── 
#>    Note. With three tests this
#>    model has as many parameters as
#>    the data can support (0
#>    residual degrees of freedom),
#>    so it reproduces the observed
#>    table exactly. Goodness-of-fit
#>    statistics are therefore
#>    omitted: they cannot tell you
#>    whether the
#>    conditional-independence
#>    assumption holds. Use four or
#>    more tests if you need to test
#>    the model's fit.
#> 
#> 
#>  Conditional Independence Check (Bivariate Residuals)  
#>  ───────────────────────────────────────────────────── 
#>    Test Pair    Bivariate Residual    Interpretation   
#>  ───────────────────────────────────────────────────── 
#>  ───────────────────────────────────────────────────── 
#>    Note. Not computable with three tests: the
#>    model has no residual degrees of freedom, so it
#>    reproduces every observed table exactly and no
#>    residual can detect conditional dependence. Add
#>    a fourth test if you need to check this
#>    assumption.
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
