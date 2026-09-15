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
#> Analysing 300 cases
#> All 300 cases have a result for every selected test.
#>  Agreement Statistics (Cohen's Kappa)                      
#>  ───────────────────────────────────────────────────────── 
#>    Test Pair         Kappa        p-value      Agreement   
#>  ───────────────────────────────────────────────────────── 
#>    Test1 vs Test2    0.1767473    0.1003799     83.66667   
#>    Test1 vs Test3    0.1067762    0.3107576     80.66667   
#>    Test2 vs Test3    0.2679128    0.0062952     84.33333   
#>  ───────────────────────────────────────────────────────── 
#>    Note. Kappa standard errors and p-values use a
#>    large-sample normal approximation rather than the
#>    exact asymptotic SE (e.g. vcd::Kappa); interpret
#>    p-values cautiously, especially in small samples.
#> 
#> 
#>  <div class='clinical-summary' style='background: #f0f8ff; padding:
#>  15px; border-radius: 8px; margin: 10px 0;'><h4 style='color: #1565c0;
#>  margin-top: 0;'> Clinical Summary
#> 
#>  Analysis: No gold standard analysis using latent_class method
#> 
#>  Tests analyzed: Test1, Test2, Test3 (N=3)
#> 
#>  Disease prevalence: 14.7%
#> 
#>  Test sensitivities: Range from 31.6% to 61.3%
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
#>     14.67025     10.66659     18.67390   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test     Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1       31.60863     17.87225     45.34501       92.15248     88.85823     95.44673     40.91527     88.68438   
#>    Test2       61.29889     46.90902     75.68875       97.64759     95.79097     99.50422     81.75180     93.62073   
#>    Test3       46.48314     31.74774     61.21853       92.36592     89.11301     95.61882     51.14393     90.94109   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. 95% intervals are normal-approximation (Wald) intervals, using the estimated number of diseased cases as
#>    the denominator for sensitivity and non-diseased for specificity. They treat the estimates as observed
#>    proportions and so understate the uncertainty of a latent-variable model; enable Bootstrap for intervals that
#>    account for the estimation itself.
#> 
#> 
#>  Model Fit Statistics                  
#>  ───────────────────────────────────── 
#>    Statistic             Value         
#>  ───────────────────────────────────── 
#>    BIC                    670.613231   
#>    AIC                    644.686753   
#>    Log-Likelihood        -315.343377   
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
