# nogoldstandard Screening Data - Five-Test Panel

Comprehensive five-test screening dataset with 250 patients. Tests
include imaging, clinical exam, biomarker, questionnaire, and AI
algorithm with varying characteristics (Sens: 0.82-0.60, Spec:
0.92-0.75).

## Usage

``` r
nogoldstandard_screening
```

## Format

A data frame with 250 rows and 8 variables:

- patient_id:

  Character: Patient identifier (PT001-PT250)

- Imaging:

  Factor: Imaging result ("Normal", "Abnormal"), Sens=0.82, Spec=0.90

- ClinicalExam:

  Factor: Clinical exam ("Normal", "Abnormal"), Sens=0.65, Spec=0.85

- Biomarker:

  Factor: Biomarker test ("Normal", "Abnormal"), Sens=0.70, Spec=0.88

- Questionnaire:

  Factor: Risk questionnaire ("Negative", "Positive"), Sens=0.60,
  Spec=0.75

- AI_Algorithm:

  Factor: AI prediction ("Negative", "Positive"), Sens=0.88, Spec=0.92

- age:

  Numeric: Patient age in years (mean 58, SD 15)

- screening_round:

  Numeric: Screening round number (1-5)

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 15% disease prevalence (screening setting). Five tests
with diverse characteristics demonstrate comprehensive evaluation
methods.

## Examples

``` r
data(nogoldstandard_screening)
nogoldstandard(data = nogoldstandard_screening,
               test1 = "Imaging", test1Positive = "Abnormal",
               test2 = "ClinicalExam", test2Positive = "Abnormal",
               test3 = "Biomarker", test3Positive = "Abnormal",
               test4 = "Questionnaire", test4Positive = "Positive",
               test5 = "AI_Algorithm", test5Positive = "Positive",
               clinicalPreset = "screening_evaluation")
#> 
#>  ANALYSIS WITHOUT GOLD STANDARD
#> WARNING: Clinical preset: screening evaluation
#> Designed for population screening test evaluation Use for evaluating screening programs with multiple tests This preset does NOT change your settings automatically -- set them yourself in the options panel: Analysis method: currently "latent_class", recommended "any_positive"; Bootstrap confidence intervals: currently off, recommended on; Bootstrap samples: currently 1000, recommended 500.
#> 
#> Analysing 250 cases
#> All 250 cases have a result for every selected test.
#>  Agreement Statistics (Cohen's Kappa)                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Test Pair                        Kappa        p-value      Agreement   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Imaging vs ClinicalExam          0.3750000    0.0000003     77.60000   
#>    Imaging vs Biomarker             0.3672457    0.0000034     79.60000   
#>    Imaging vs Questionnaire         0.0000000          NaN      0.00000   
#>    Imaging vs AI_Algorithm          0.0000000          NaN      0.00000   
#>    ClinicalExam vs Biomarker        0.2783551    0.0003003     74.00000   
#>    ClinicalExam vs Questionnaire    0.0000000          NaN      0.00000   
#>    ClinicalExam vs AI_Algorithm     0.0000000          NaN      0.00000   
#>    Biomarker vs Questionnaire       0.0000000          NaN      0.00000   
#>    Biomarker vs AI_Algorithm        0.0000000          NaN      0.00000   
#>    Questionnaire vs AI_Algorithm    0.2058590    0.0091408     71.20000   
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
#>  Tests analyzed: Imaging, ClinicalExam, Biomarker, Questionnaire,
#>  AI_Algorithm (N=5)
#> 
#>  Disease prevalence: 18.1%
#> 
#>  Test sensitivities: Range from 53.2% to 87.2%
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
#>     18.14302     13.36596     22.92008   
#>  ─────────────────────────────────────── 
#> 
#> 
#>  Test Performance Metrics                                                                                                      
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test             Sensitivity    Lower CI     Upper CI     Specificity    Lower CI     Upper CI     PPV          NPV         
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Imaging             81.75046     70.50974     92.99119       93.68655     90.35442     97.01868     74.16001     95.86123   
#>    ClinicalExam        76.51191     64.17485     88.84897       84.70695     79.77570     89.63820     52.58170     94.20999   
#>    Biomarker           64.29725     50.35380     78.24071       89.32951     85.09952     93.55951     57.18364     91.86237   
#>    Questionnaire       53.17036     38.64863     67.69210       79.04480     73.46868     84.62093     35.99518     88.39302   
#>    AI_Algorithm        87.20045     77.47791     96.92300       94.40585     91.25725     97.55444     77.55291     97.08263   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. 95% intervals are normal-approximation (Wald) intervals, using the estimated number of diseased cases as the
#>    denominator for sensitivity and non-diseased for specificity. They treat the estimates as observed proportions and so
#>    understate the uncertainty of a latent-variable model; enable Bootstrap for intervals that account for the estimation
#>    itself.
#> 
#> 
#>  Model Fit Statistics                 
#>  ──────────────────────────────────── 
#>    Statistic             Value        
#>  ──────────────────────────────────── 
#>    BIC                   1218.82552   
#>    AIC                   1180.08945   
#>    Log-Likelihood        -579.04473   
#>    G-squared               14.60707   
#>    Chi-squared             11.77809   
#>    Degrees of Freedom      20.00000   
#>  ──────────────────────────────────── 
#> 
#> 
#>  Conditional Independence Check (Bivariate Residuals)                                    
#>  ─────────────────────────────────────────────────────────────────────────────────────── 
#>    Test Pair                        Bivariate Residual    Interpretation                 
#>  ─────────────────────────────────────────────────────────────────────────────────────── 
#>    Imaging vs ClinicalExam                  0.15185514    Consistent with independence   
#>    Imaging vs Biomarker                     0.14793587    Consistent with independence   
#>    Imaging vs Questionnaire                 0.35557163    Consistent with independence   
#>    Imaging vs AI_Algorithm                  0.40176613    Consistent with independence   
#>    ClinicalExam vs Biomarker                0.13339412    Consistent with independence   
#>    ClinicalExam vs Questionnaire            0.10335599    Consistent with independence   
#>    ClinicalExam vs AI_Algorithm             0.06225217    Consistent with independence   
#>    Biomarker vs Questionnaire               0.43600437    Consistent with independence   
#>    Biomarker vs AI_Algorithm                0.16860161    Consistent with independence   
#>    Questionnaire vs AI_Algorithm            0.06687499    Consistent with independence   
#>  ─────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. A residual above 3.84 (the 5% point of chi-squared on 1 degree of freedom)
#>    is evidence that the pair does not err independently. This is a descriptive
#>    check, not a formal test: the residuals are correlated with one another and no
#>    multiplicity adjustment is applied.
#> 
#> 
#>  Test Cross-Tabulation                                                                         
#>  ───────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test Combination                                                      Count    Percentage   
#>  ───────────────────────────────────────────────────────────────────────────────────────────── 
#>    Imaging-, ClinicalExam-, Biomarker-, Questionnaire-, AI_Algorithm-      108      43.20000   
#>    Imaging-, ClinicalExam-, Biomarker-, Questionnaire+, AI_Algorithm-       30      12.00000   
#>    Imaging-, ClinicalExam+, Biomarker-, Questionnaire-, AI_Algorithm-       18       7.20000   
#>    Imaging-, ClinicalExam-, Biomarker+, Questionnaire-, AI_Algorithm-       13       5.20000   
#>    Imaging+, ClinicalExam+, Biomarker+, Questionnaire+, AI_Algorithm+       11       4.40000   
#>    Imaging+, ClinicalExam-, Biomarker-, Questionnaire-, AI_Algorithm-        8       3.20000   
#>    Imaging-, ClinicalExam-, Biomarker-, Questionnaire-, AI_Algorithm+        7       2.80000   
#>    Imaging-, ClinicalExam+, Biomarker-, Questionnaire+, AI_Algorithm-        5       2.00000   
#>    Imaging+, ClinicalExam+, Biomarker-, Questionnaire-, AI_Algorithm+        5       2.00000   
#>    Imaging-, ClinicalExam-, Biomarker+, Questionnaire+, AI_Algorithm-        4       1.60000   
#>    Imaging-, ClinicalExam+, Biomarker-, Questionnaire-, AI_Algorithm+        4       1.60000   
#>    Imaging+, ClinicalExam+, Biomarker+, Questionnaire-, AI_Algorithm+        4       1.60000   
#>    Imaging+, ClinicalExam+, Biomarker-, Questionnaire+, AI_Algorithm+        4       1.60000   
#>    Imaging+, ClinicalExam+, Biomarker-, Questionnaire-, AI_Algorithm-        3       1.20000   
#>    Imaging-, ClinicalExam+, Biomarker+, Questionnaire-, AI_Algorithm-        3       1.20000   
#>    Imaging+, ClinicalExam-, Biomarker+, Questionnaire-, AI_Algorithm+        3       1.20000   
#>    Imaging-, ClinicalExam+, Biomarker+, Questionnaire-, AI_Algorithm+        3       1.20000   
#>    Imaging+, ClinicalExam-, Biomarker+, Questionnaire+, AI_Algorithm+        3       1.20000   
#>    Imaging+, ClinicalExam+, Biomarker+, Questionnaire-, AI_Algorithm-        2       0.80000   
#>    Imaging+, ClinicalExam-, Biomarker-, Questionnaire+, AI_Algorithm-        2       0.80000   
#>    Imaging+, ClinicalExam-, Biomarker-, Questionnaire+, AI_Algorithm+        2       0.80000   
#>    Imaging+, ClinicalExam-, Biomarker+, Questionnaire+, AI_Algorithm-        1       0.40000   
#>    Imaging-, ClinicalExam+, Biomarker+, Questionnaire+, AI_Algorithm-        1       0.40000   
#>    Imaging+, ClinicalExam+, Biomarker+, Questionnaire+, AI_Algorithm-        1       0.40000   
#>    Imaging+, ClinicalExam-, Biomarker-, Questionnaire-, AI_Algorithm+        1       0.40000   
#>    Imaging-, ClinicalExam-, Biomarker+, Questionnaire-, AI_Algorithm+        1       0.40000   
#>    Imaging-, ClinicalExam-, Biomarker-, Questionnaire+, AI_Algorithm+        1       0.40000   
#>    Imaging-, ClinicalExam+, Biomarker-, Questionnaire+, AI_Algorithm+        1       0.40000   
#>    Imaging-, ClinicalExam+, Biomarker+, Questionnaire+, AI_Algorithm+        1       0.40000   
#>    Imaging+, ClinicalExam-, Biomarker+, Questionnaire-, AI_Algorithm-        0       0.00000   
#>    Imaging+, ClinicalExam+, Biomarker-, Questionnaire+, AI_Algorithm-        0       0.00000   
#>    Imaging-, ClinicalExam-, Biomarker+, Questionnaire+, AI_Algorithm+        0       0.00000   
#>  ───────────────────────────────────────────────────────────────────────────────────────────── 
#> 


```
