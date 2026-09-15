# psychopdaROC Perfect Separation Data

Dataset with 100 patients showing perfect discrimination between
positive and negative cases (AUC = 1.0), useful for testing edge case
handling.

## Usage

``` r
psychopdaROC_perfect
```

## Format

A data frame with 100 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT100)

- condition:

  Factor: "Positive" or "Negative" (50%/50% prevalence)

- perfect_test:

  Numeric: Test values (80-100 for positive, 0-20 for negative)

## Source

Generated test data for ClinicoPath package

## Details

Complete separation between classes with no overlap. Positive cases have
values uniformly distributed between 80-100, negative cases between
0-20. Tests ability to handle perfect discrimination scenarios.

## Examples

``` r
data(psychopdaROC_perfect)
psychopdaROC(data = psychopdaROC_perfect, dependentVars = "perfect_test",
             classVar = "condition", positiveClass = "Positive",
             refVar = "perfect_test")
#> Warning: ci.auc() of a ROC curve with AUC == 1 is always 1-1 and can be misleading.
#> Warning: var() of a ROC curve with AUC == 1 is always 0 and can be misleading.
#> Warning: ci.auc() of a ROC curve with AUC == 1 is always 1-1 and can be misleading.
#> Warning: var() of a ROC curve with AUC == 1 is always 0 and can be misleading.
#> 
#>  ADVANCED ROC ANALYSIS
#> 
#> 
#> 
#> 
#>  Procedure Notes
#> 
#> 
#> 
#>  The ROC analysis has been completed using the following
#>  specifications:
#> 
#>   
#> 
#>  Measure Variable(s): perfect_test
#> 
#>  Class Variable: condition
#> 
#>  Positive Class: Positive
#> 
#>   
#> 
#>  Method: maximize_metric
#> 
#>  All Observed Cutpoints: FALSE
#> 
#>  Metric: youden
#> 
#>  Direction (relative to cutpoint): >=
#> 
#>  Tie Breakers: mean
#> 
#>  Metric Tolerance: 1e-06
#> 
#>   
#> 
#>  <hr />
#> 
#>  <div style='padding: 10px; background-color: #f8f9fa; border: 1px
#>  solid #dee2e6; border-radius: 4px; margin-bottom: 15px;'>
#> 
#>  Analysis Status
#> 
#>  Seed: 123Positive Class: Positive (Prevalence: 44%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                       
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Variable        AUC          95% CI Lower    95% CI Upper    p-value     
#>  ────────────────────────────────────────────────────────────────────────── 
#>    perfect_test    1.0000000       1.0000000       1.0000000                
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Positive</b> (Classification Direction = ">="). If that is
#>    the wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification Direction
#>    to "<=" and the AUC becomes 1 minus the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                                            
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test            Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                                              
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    perfect_test    Excellent            Suitable for clinical use with appropriate cutpoint    The test 'perfect_test' has an AUC of 1.000 indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients.   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    80.4946577      100.00000      100.00000    100.00000    100.00000     1.0000000    1.0000000       1.0000000   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                   
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Variable        AUC          95% CI Lower    95% CI Upper    p-value     
#>  ────────────────────────────────────────────────────────────────────────── 
#>    perfect_test    1.0000000       1.0000000       1.0000000                
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Positive</b> (Classification Direction = ">="). If that is
#>    the wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification Direction
#>    to "<=" and the AUC becomes 1 minus the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
