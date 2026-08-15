# psychopdaROC Cost-Benefit Data

Dataset with 160 patients including cost information for false positive
and false negative errors, enabling cost-benefit optimal cutpoint
determination.

## Usage

``` r
psychopdaROC_costbenefit
```

## Format

A data frame with 160 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT160)

- outcome:

  Factor: "Event" or "No_Event" (28%/72% prevalence)

- risk_score:

  Numeric: Risk prediction score (mean: 75 for event, 50 for no event)

- false_positive_cost:

  Numeric: Cost of false positive (\$100)

- false_negative_cost:

  Numeric: Cost of false negative (\$1000)

## Source

Generated test data for ClinicoPath package

## Details

False negative cost is 10x higher than false positive cost, representing
clinical scenarios where missing a case is much more costly than a false
alarm. Enables cost-ratio optimal cutpoint method.

## Examples

``` r
data(psychopdaROC_costbenefit)
psychopdaROC(data = psychopdaROC_costbenefit, dependentVars = "risk_score",
             classVar = "outcome", positiveClass = "Event",
             refVar = "risk_score",
             method = "oc_cost_ratio")
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
#>  Measure Variable(s): risk_score
#> 
#>  Class Variable: outcome
#> 
#>  Positive Class: Event
#> 
#>   
#> 
#>  Method: oc_cost_ratio
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
#>  Seed: 123Positive Class: Event (Prevalence: 26.2%)Analysis Mode: Basic
#> 
#>  ROC Analysis Summary                                                      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    risk_score    0.7781477       0.6980973       0.8594088    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Event</b> (Classification Direction = ">="). If that is
#>    the wrong way round for this marker, every sensitivity,
#>    specificity, cutpoint and AUC below is reversed — switch
#>    Classification Direction to "<=" and the AUC becomes 1 minus the
#>    value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                         
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test          Performance Level    Clinical Recommendation                            Detailed Interpretation                                                                                                                 
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    risk_score    Fair                 May be useful in combination with other markers    The test 'risk_score' has an AUC of 0.778 indicating fair discriminatory ability. Consider combining with other clinical information.   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    76.3000000       50.00000       89.83051     63.63636     83.46457     0.3983051    0.7781477       0.3983051   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                  
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    risk_score    0.7781477       0.6980973       0.8594088    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Event</b> (Classification Direction = ">="). If that is
#>    the wrong way round for this marker, every sensitivity,
#>    specificity, cutpoint and AUC below is reversed — switch
#>    Classification Direction to "<=" and the AUC becomes 1 minus the
#>    value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
