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
#> WARNING: Clinical preset: diagnostic validation
#> Recommended for validating new diagnostic tests against existing standards Use when evaluating new biomarkers or diagnostic technologies This preset does NOT change your settings automatically -- set them yourself in the options panel: Bootstrap confidence intervals: currently off, recommended on.
#> 
#> Analysing 190 cases
#> All 190 cases have a result for every selected test.
#>  Agreement Statistics (Cohen's Kappa)                                 
#>  ──────────────────────────────────────────────────────────────────── 
#>    Test Pair                   Kappa        p-value       Agreement   
#>  ──────────────────────────────────────────────────────────────────── 
#>    New_Test vs Reference1      0.5888408    < .0000001     80.52632   
#>    New_Test vs Reference2      0.6156675    < .0000001     82.63158   
#>    Reference1 vs Reference2    0.5528360    < .0000001     78.94737   
#>  ──────────────────────────────────────────────────────────────────── 
#>    Note. Kappa standard errors and p-values use a large-sample
#>    normal approximation rather than the exact asymptotic SE (e.g.
#>    vcd::Kappa); interpret p-values cautiously, especially in
#>    small samples.
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using latent_class method
#> 
#>  Tests analyzed: New_Test, Reference1, Reference2 (N=3)
#> 
#>  Disease prevalence: 34.5%
#> 
#>  Test sensitivities: Range from 83.4% to 90.9%
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
#>     34.46658     27.70884     41.22433   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                                   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test          Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    New_Test         88.50060     80.77409     96.22710       92.73649     88.17781     97.29516     86.50141     93.87760   
#>    Reference1       90.85540     83.87421     97.83660       85.14059     78.89305     91.38813     76.27952     94.65315   
#>    Reference2       83.36826     74.34961     92.38691       92.44657     87.80508     97.08806     85.30462     91.35592   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. 95% intervals are normal-approximation (Wald) intervals, using the estimated number of diseased cases as the
#>    denominator for sensitivity and non-diseased for specificity. They treat the estimates as observed proportions and
#>    so understate the uncertainty of a latent-variable model; enable Bootstrap for intervals that account for the
#>    estimation itself.
#> 
#> 
#>  Model Fit Statistics                  
#>  ───────────────────────────────────── 
#>    Statistic             Value         
#>  ───────────────────────────────────── 
#>    BIC                    624.352992   
#>    AIC                    601.623823   
#>    Log-Likelihood        -293.811912   
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
