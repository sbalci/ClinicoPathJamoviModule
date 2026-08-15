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
#> WARNING: Excluded 30 case(s) with missing test results
#> This analysis uses the 120 of 150 cases (80.0%) with a result recorded for every selected test. Latent class and composite estimates assume the excluded cases are missing at random; if a test is more often missing when it would have been positive, the estimates below are biased.
#>  Agreement Statistics (Cohen's Kappa)                       
#>  ────────────────────────────────────────────────────────── 
#>    Test Pair         Kappa        p-value       Agreement   
#>  ────────────────────────────────────────────────────────── 
#>    Test1 vs Test2    0.5472441    < .0000001     80.83333   
#>    Test1 vs Test3    0.4512195     0.0000012     77.50000   
#>    Test2 vs Test3    0.3957704     0.0000343     75.00000   
#>  ────────────────────────────────────────────────────────── 
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
#>  Disease prevalence: 31.9%
#> 
#>  Test sensitivities: Range from 64.8% to 82.8%
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
#>     31.93134     23.58993     40.27275   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test     Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1       82.81741     70.87329     94.76153       94.77690     89.95188     99.60192     88.14903     92.16196   
#>    Test2       77.58385     64.37956     90.78814       91.09756     84.92178     97.27334     80.34669     89.65141   
#>    Test3       64.83313     49.71444     79.95183       90.01316     83.51109     96.51522     75.28032     84.51138   
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
#>    BIC                    406.672567   
#>    AIC                    387.160125   
#>    Log-Likelihood        -186.580063   
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
