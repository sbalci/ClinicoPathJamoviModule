# psychopdaROC Subgroup Analysis Data

Dataset for subgroup ROC analysis with 200 patients stratified by age
group and sex, allowing evaluation of test performance across
demographic subgroups.

## Usage

``` r
psychopdaROC_subgroup
```

## Format

A data frame with 200 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT200)

- disease:

  Factor: "Disease" or "Healthy" (30%/70% prevalence)

- test_score:

  Numeric: Diagnostic test score (mean: 70 for disease, 45 for healthy)

- age_group:

  Factor: "Young", "Middle", or "Elderly"

- sex:

  Factor: "Male" or "Female"

## Source

Generated test data for ClinicoPath package

## Details

Enables subgroup-specific ROC analysis to assess whether test
performance varies across age groups or between sexes. Useful for
evaluating test generalizability across populations.

## Examples

``` r
data(psychopdaROC_subgroup)
psychopdaROC(data = psychopdaROC_subgroup,
             dependentVars = "test_score", classVar = "disease",
             positiveClass = "Disease", refVar = "test_score",
             subGroup = "age_group")
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
#>  Measure Variable(s): test_score
#> 
#>  Class Variable: disease
#> 
#>  Positive Class: Disease
#> 
#>  Subgroup Variable: age_group
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
#>  Seed: 123Positive Class: Disease (Prevalence: 28.5%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                                  
#>  ───────────────────────────────────────────────────────────────────────────────────── 
#>    Variable                  AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────────────────── 
#>    test_score ::: Elderly    0.7889610       0.6760838       0.9018383     0.0000005   
#>    test_score ::: Young      0.9189542       0.8487080       0.9892005    < .0000001   
#>    test_score ::: Middle     0.8869396       0.8113542       0.9625250    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to indicate
#>    Disease</b> (Classification Direction = ">="). If that is the wrong way round
#>    for this marker, every sensitivity, specificity, cutpoint and AUC below is
#>    reversed — switch Classification Direction to "<=" and the AUC becomes 1 minus
#>    the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 
#> 
#>  Clinical Interpretation                                                             
#>  ─────────────────────────────────────────────────────────────────────────────────── 
#>    Test    Performance Level    Clinical Recommendation    Detailed Interpretation   
#>  ─────────────────────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    55.9136561       85.71429       61.36364     51.42857     90.00000     0.4707792    0.7889610       0.4707792   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    62.3896532       88.23529       84.44444     68.18182     95.00000     0.7267974    0.9189542       0.7267974   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    50.3821603       94.73684       79.62963     62.06897     97.72727     0.7436647    0.8869396       0.7436647   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                              
#>  ───────────────────────────────────────────────────────────────────────────────────── 
#>    Variable                  AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────────────────── 
#>    test_score ::: Elderly    0.7889610       0.6760838       0.9018383     0.0000005   
#>    test_score ::: Young      0.9189542       0.8487080       0.9892005    < .0000001   
#>    test_score ::: Middle     0.8869396       0.8113542       0.9625250    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to indicate
#>    Disease</b> (Classification Direction = ">="). If that is the wrong way round
#>    for this marker, every sensitivity, specificity, cutpoint and AUC below is
#>    reversed — switch Classification Direction to "<=" and the AUC becomes 1 minus
#>    the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
